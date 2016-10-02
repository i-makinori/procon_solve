(in-package #:procon)


(defstruct piece
  (vectors '())
  (degrees '()))



(defun degree-adjust (degrees)
  "in n-polygon. when degree-rength is not (n-2)*pi, let degree-length (n-2)*pi "
  (let* ((length (length degrees))
         (adjust-func
          (if (a-d= (* PI (- length 2)) (reduce #'+ degrees))
              #'(lambda (d) d)
              #'(lambda (d) (- *2pi* d)))))
    (mapcar adjust-func degrees)))

(defun clock-wise-angle (vec1 vec2 vec3)
  "vec1 from to vec2"
  (let ((angle (vectors-to-angle vec1 vec2 vec3)))
    (if (> angle 0) angle
        (+ PI angle)
  )))

(defun coord-to-piece (coordinates)
  (let* ((vectors (mapcar #'(lambda (c1) (dxdy (car coordinates) c1)) coordinates))
         ;;(vectors (map-tuple/c #'dxdy 2 coordinates))
         (degrees (degree-adjust
                   (map-tuple/c #'clock-wise-angle 3
                                (rotate-list coordinates -1)))))
    (make-piece
     :vectors vectors
     :degrees degrees)))


;;;; state
(defstruct piece-condition 
  "piece : piece,
piece-point : num(0 to x)
two-side : (T:surface, nil:back),
angle: float(-xPI to xPI)"
  (piece '())
  (point 0)
  (two-side 't)
  (angle 0))


;; hit-judge
(defun point-piece-include-detection (piece-lines point-vec)
  "nil : not-included
t : included"
  (let ((judged-line1 (vector-to-line (vec *-huge-num* (vy point-vec)) point-vec))
        (judged-line2 (vector-to-line (vec *huge-num*  (1+ (vy point-vec)))
                                      (vec (+  2 (vx point-vec)) (+ 2 (vy point-vec)))))
        )
    (not (some #'(lambda (line)
                   (evenp  (length
                            (remove nil
                                    (mapcar #'(lambda (l)
                                                (line-collision-detection l line))
                                            piece-lines)))))
               (list judged-line1 judged-line2)
               ))))

(defun point-included-in-piece (piece)
  "search each triangle included in piece,
center-deg is smaller than (pi - st-error), it's gravity-center is included in piece"
  (let ((n (search '(()) (piece-degrees piece)
                   :test #'(lambda (n x) n (> (* pi 3/4) x))))
        (vecs (piece-vectors piece)))
    (gravity-center (list
                     (rotate-nth (- n 1) vecs)
                     (nth n vecs)
                     (rotate-nth (+ n 1) vecs)))))


(defun piece-collision-detection (piece1 piece2)
  "piece hit-judge"
  (let ((piece1-vectors (piece-vectors piece1))
        (piece2-vectors (piece-vectors piece2)))
    (let ((lines1 (map-tuple/c #'vector-to-line 2 piece1-vectors))
          (lines2 (map-tuple/c #'vector-to-line 2 piece2-vectors))
          )
      (or
       (point-piece-include-detection lines2 (point-included-in-piece piece1))
       (point-piece-include-detection lines1 (point-included-in-piece piece2))
       (not (every #'(lambda (line2)
                       (notany #'(lambda (line1)
                                   (line-collision-detection line1 line2))
                               lines1))
                   lines2))))))

;;;; synthesize

(defun synthesize-piece (piece1-condition piece2-condition)
  (let ((refed-piece1 (refrect-piece-condi piece1-condition))
        (refed-piece2 (refrect-piece-condi piece2-condition)))
    (if (piece-collision-detection refed-piece1
                                   refed-piece2)
        nil
        (synthesize-piece-rule refed-piece1 refed-piece2))))

;;;; deploy

(defun refrect-piece-condi (piece-condi)
  (rotate-piece
   (turn-orver-piece
    (origin-shift (piece-condition-piece piece-condi) (piece-condition-point piece-condi))
    (piece-condition-two-side piece-condi)) (piece-condition-angle piece-condi)))

(defun rotate-piece (piece angle)
  (make-piece
   :vectors (mapcar #'(lambda (v) 
                        (vec (round (- (* (vx v) (cos angle)) (* (vy v) (sin angle))))
                             (round (+ (* (vx v) (sin angle)) (* (vy v) (cos angle ))))))
                    (piece-vectors piece))
   :degrees (piece-degrees piece)))

(defun turn-orver-piece (piece two-side)
  "turn orver in y-axis 
nil:turnout, t:surfece"
  (if two-side piece
      (make-piece 
       :vectors (mapcar #'(lambda (v) (vec (- (vx v)) (vy v))) (piece-vectors piece))
       :degrees (piece-degrees piece)
       )))

(defun origin-shift (piece point)
  (let ((new-origin (nth point (piece-vectors piece))))
    (make-piece
     :vectors (rotate-list (mapcar #'(lambda (p) (vec-sub p new-origin))
                                   (piece-vectors piece))
                           point)
     :degrees (rotate-list (piece-degrees piece) point))))


 
