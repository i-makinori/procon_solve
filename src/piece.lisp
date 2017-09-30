(in-package #:procon)


(defstruct piece16-
  (vectors '())
  (degrees '()))





(defun coord-to-piece16- (coordinates)
  (let* ((vectors (mapcar #'(lambda (c1) (dxdy (car coordinates) c1)) coordinates))
         ;;(vectors (map-tuple/c #'dxdy 2 coordinates))
         (degrees (degree-adjust
                   (map-tuple/c #'clock-wise-angle 3
                                (rotate-list coordinates -1)))))
    (make-piece16-
     :vectors vectors
     :degrees degrees)))


;;;; state
(defstruct piece16--condition 
  "piece16- : piece16-,
piece16--spot : num(0 to x)
two-side : (T:surface, nil:back),
angle: float(-xPI to xPI)"
  (piece16- '())
  (spot 0)
  (two-side 't)
  (angle 0))


;; hit-judge
(defun spot-piece16--include-detection (piece16--lines spot-vec)
  "nil : not-included
t : included"
  (let ((judged-line1 (vector-to-line (vec *-huge-num* (vy spot-vec)) spot-vec))
        (judged-line2 (vector-to-line (vec *huge-num*  (1+ (vy spot-vec)))
                                      (vec (+ 2 (vx spot-vec)) (+ 2 (vy spot-vec))))))
    (not (some #'(lambda (line)
                   (evenp (length
                           (remove nil
                                   (mapcar #'(lambda (l)
                                               (line-collision-detection l line))
                                           piece16--lines)))))
               (list judged-line1 judged-line2)))))


(defun spot-included-in-piece16- (piece16-)
  "search each triangle included in piece16-,
center-deg is smaller than (pi - st-error), it's gravity-center is included in piece16-"
  (let ((n (search '(()) (piece16--degrees piece16-)
                   :test #'(lambda (n x) n (> pi x))))
        (vecs (piece16--vectors piece16-)))
    (gravity-center (list
                     (rotate-nth (- n 1) vecs)
                     (nth n vecs)
                     (rotate-nth (+ n 1) vecs)))))

(defun lines-lines-hit-judge (lines1 lines2)
  (not (every #'(lambda (line2)
                  (notany #'(lambda (line1)
                              (line-collision-detection-error line1 line2))
                          lines1))
              lines2)))


(defun piece16--collision-detection (piece16-1 piece16-2)
  "piece16- hit-judge"
  (let ((piece16-1-vectors (piece16--vectors piece16-1))
        (piece16-2-vectors (piece16--vectors piece16-2)))
    (let ((lines1 (map-tuple/c #'vector-to-line 2 piece16-1-vectors))
          (lines2 (map-tuple/c #'vector-to-line 2 piece16-2-vectors)))
      (or
       (spot-piece16--include-detection lines2 (spot-included-in-piece16- piece16-1))
       (spot-piece16--include-detection lines1 (spot-included-in-piece16- piece16-2))
       (lines-lines-hit-judge lines1 lines2)))))

;;;; synthesize

(defun synthesize-piece16- (piece16-1-condition piece16-2-condition)
  (let ((refed-piece16-1 (refrect-piece16--condi piece16-1-condition))
        (refed-piece16-2 (refrect-piece16--condi piece16-2-condition)))
    (if (piece16--collision-detection refed-piece16-1
                                      refed-piece16-2)
        nil
        (synthesize-piece16--rule refed-piece16-1 refed-piece16-2))))

(defun piece16--condition-collision-detection (condi1 condi2)
  (piece16--collision-detection (refrect-piece16--condi condi1)
                                (refrect-piece16--condi condi2)))

(defun let-synthesize-piece16--condition (condi1 condi2)
  (synthesize-piece16--rule (refrect-piece16--condi condi1)
                            (refrect-piece16--condi condi2)))


;;;; deploy

(defun refrect-piece16--condi (piece16--condi)
  (rotate-piece16-
   (turn-orver-piece16-
    (origin-shift (piece16--condition-piece16- piece16--condi) (piece16--condition-spot piece16--condi))
    (piece16--condition-two-side piece16--condi)) (piece16--condition-angle piece16--condi)))

(defun rotate-piece16- (piece16- angle)
  (make-piece16-
   :vectors (mapcar #'(lambda (v) 
                        (vec (- (* (vx v) (cos angle)) (* (vy v) (sin angle)))
                             (+ (* (vx v) (sin angle)) (* (vy v) (cos angle )))))
                    (piece16--vectors piece16-))
   :degrees (piece16--degrees piece16-)))

(defun turn-orver-piece16- (piece16- two-side)
  "turn orver in y-axis 
nil:turnout, t:surfece"
  (if two-side piece16-
      (make-piece16- 
       :vectors (mapcar #'(lambda (v) (vec (- (vx v)) (vy v))) (piece16--vectors piece16-))
       :degrees (piece16--degrees piece16-)
       )))

(defun origin-shift (piece16- spot)
  (let ((new-origin (nth spot (piece16--vectors piece16-))))
    (make-piece16-
     :vectors (rotate-list (mapcar #'(lambda (p) (vec-sub p new-origin))
                                   (piece16--vectors piece16-))
                           spot)
     :degrees (rotate-list (piece16--degrees piece16-) spot))))

;;;; util
(defun piece16--vector-rounds (piece16-)
  (mapcar #'(lambda (pair)
              (cons (round (car pair)) (round (cdr pair))))
          (piece16--vectors piece16-)))

(defun piece16-2-delta-high (piece16-1 piece16-2)
  (let* ((piece16-1-vectors (piece16--vectors piece16-1))
         (piece16-2-vectors (piece16--vectors piece16-2))
         (piece16-1-delta-h
          (- (cdar (stable-sort piece16-1-vectors
                                #'(lambda (v1 v2) (> (cdr v1) (cdr v2)))))
             (cdar (stable-sort piece16-1-vectors
                                #'(lambda (v1 v2) (< (cdr v1) (cdr v2)))))))
         (piece16-2-delta-h
          (- (cdar (stable-sort piece16-2-vectors
                                #'(lambda (v1 v2) (> (cdr v1) (cdr v2)))))
             (cdar (stable-sort piece16-2-vectors
                                #'(lambda (v1 v2) (< (cdr v1) (cdr v2)))))))
         )
    (max piece16-1-delta-h piece16-2-delta-h)))
