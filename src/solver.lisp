
(in-package :puzzle-1617)

;;;; piece

(defun point (x y)
  (vec3 x y 1))

;;;; nomial

;;;; polynomial

;;;; vector, matrix

;;;; transforms



;;;; polynomial


;;; sample problem
(defparameter *shape1* (list #(0 0 1) #(18 0 1) #(16 8 1) #(18 18 1) #(9 18 1) #(9 17 1) #(2 16 1)
                             #(2 18 1) #(0 18 1) #(0 14 1) #(2 11 1) #(1 8 1) #(3 3 1) #(0 2 1)))

(defparameter *example-problem-10*
  (load-problem-file-into-puzzle "puzzle_10.txt"))


;;;; collision detection

;;; point on line-segment

(defun point-on-line-segment-detection (point point1 point2)
  ;; p = p1 + T*p1p2 = p1 + T*(p2-p1) ;; points set on line expressed by vector.
  ;; if 0 < T < 1, p rides the line-segment.
  ;; D1 is polynomial of line (p1, p2).
  ;; if D1 is near to 0, point is near to line.
  (let ((D1 (+ (+ (* (- (vec3-y point2) (vec3-y point1)) (vec3-x point)))
               (- (* (- (vec3-x point2) (vec3-x point1)) (vec3-y point)))
               (- (vec3-cross-xy point1 point2))))
        (Tx (/ (- (vec3-x point)  (vec3-x point1))
               (- (vec3-x point2) (vec3-x point1))))
        (Ty (/ (- (vec3-y point)  (vec3-y point1))
               (- (vec3-y point2) (vec3-y point1)))))
    (and (ser= D1 0) ;; almost on-line
         (< (+ 0 *standard-error*) Tx (- 1 *standard-error*)) ;; on-x range. but not near ε.
         (< (+ 0 *standard-error*) Ty (- 1 *standard-error*)) ;; on-x range. but not near ε.
  )))

;;; collision detection for boundary lines

(defun point-line-collision-detection (point-a1 point-a2 point-b1 point-b2)
  ;; {   0 < (A1A2 X A1B1) * (A1A2 X A1B2)
  ;;  && 0 < (B1B2 X B1A1) * (B1B2 X B1A2)}
  ;; => 2D line A1A2 and B1B2 are crossing.
  ;; Ref: https://qiita.com/zu_rin/items/e04fdec4e3dec6072104
  (let ((vec-a1a2 (vec3-sub-xy point-a2 point-a1))
        (vec-b1b2 (vec3-sub-xy point-b2 point-b1)))
    (and
     (> (- -0.0001 *standard-error*) ;; round tolerance, remove line-on points.
        (* (vec3-cross-xy vec-a1a2 (vec3-sub-xy point-b1 point-a1))   ;; A1A2 X A1B1
           (vec3-cross-xy vec-a1a2 (vec3-sub-xy point-b2 point-a1)))) ;; A1A2 X A1B2
     (> (- -0.0001 *standard-error*) ;; round tolerance, remove line-on points.
        (* (vec3-cross-xy vec-b1b2 (vec3-sub-xy point-a1 point-b1))   ;; B1B2 X B1A1
           (vec3-cross-xy vec-b1b2 (vec3-sub-xy point-a2 point-b1)))) ;; B1B2 X B1A2
     )))

(defun piece-piece-boundary-collision-detection (piece1 piece2)
  (labels ((some-line-collisions-p (papb pnpms)
             (cond ((null pnpms) nil)
                   ((point-line-collision-detection (car papb) (cdr papb) (caar pnpms) (cdar pnpms))
                    t)
                   (t nil))))
  (let* ((tu-points1 (make-tuple-list (piece-points piece1)))
         (tu-points2 (make-tuple-list (piece-points piece2))))
    ;; if some edge by 2 points crosses to another edges, piece1 and piece2 are touch crossing.
    (or (some #'(lambda (p1ap1b) (some-line-collisions-p p1ap1b tu-points2)) tu-points1)
        (some #'(lambda (p2ap2b) (some-line-collisions-p p2ap2b tu-points1)) tu-points2)))))

;;;; contains detection for aprrox point.

(defun collision-detection-piece-piece (p1 p2)
  ;; and boundary contain
  p1 p2 nil
  )

#|
(defun point-inner-domnain-p (point shape)
  "shape is vec3 list of piece."
  ;; does not contains above line points. because direction > 0 .
  ;; ref: http://www.sousakuba.com/Programming/gs_hittest_point_triangle.html
  (let* ((edge-vecs (map-tuple #'(lambda (coord1 coord2) (vec3-sub-xy coord2 coord1)) shape))
         (to-point-vecs (mapcar #'(lambda (coord) (vec3-sub-xy coord point)) shape))
         (crosses-xy (mapcar #'vec3-cross-xy edge-vecs to-point-vecs)))
    (or (every #'(lambda (dir) (> dir 0)) crosses-xy)
        (every #'(lambda (dir) (< dir 0)) crosses-xy))

    ))
|#

(defun angle (dir-vec-ab dir-vec-ac)
  "need dir-vec-a* to be starts from common point a"
  ;; theta    = Acos(cos(theta))
  ;; a・b     = |a|*|b|*cos(theta)
  ;; ∴ theta = Acos(a・b / |a|*|b|) = Acos(unit(a)・unit(b))
  (realpart (acos (/ (vec3-dot-xy dir-vec-ab dir-vec-ac)
                     (* (vec3-length-xy dir-vec-ab) (vec3-length-xy dir-vec-ac)))))
  ;;(acos (vec3-dot-xy (vec3-normalize-xy dir-vec-ab)
  ;;(vec3-normalize-xy dir-vec-ac)))
  )

(defun point-inner-domnain-p (point shape)
  "shape is vec3 list of piece."
  ;; ?? does not contains above line points. because direction > 0 . ??
  ;; ref: https://www.nttpc.co.jp/technology/number_algorithm.html
  (let*
      ((vec-point-to-point (mapcar #'(lambda (coord) (vec3-sub-xy coord point)) shape))
       (angles (map-tuple #'angle vec-point-to-point)))
    (>= (abs (apply #'+ angles)) *pi*2*)
    ))


(defun shape-domain-rect (shape)
  "(list x-min y-min x-max y-max)"
  (let ((x-coords (mapcar #'vec3-x shape))
        (y-coords (mapcar #'vec3-y shape)))
    (mapcar #'(lambda (fs) (apply (car fs) (cdr fs)))
            (list (cons #'min x-coords) (cons #'min y-coords)
                  (cons #'max x-coords) (cons #'max y-coords)))))

(defun domain-rect-x-min (domain-rect) (nth 0 domain-rect))
(defun domain-rect-y-min (domain-rect) (nth 1 domain-rect))
(defun domain-rect-x-max (domain-rect) (nth 2 domain-rect))
(defun domain-rect-y-max (domain-rect) (nth 3 domain-rect))

(defparameter *default-approx-length* (* 1 1/2))

(defun approx-points-list (domain-rect &optional (approx-length *default-approx-length*))
  "domain :: (list x_min y_min x_max y_max)"
  (let ((y-from (+ (domain-rect-y-min domain-rect) (/ approx-length 2)))
        (y-to   (- (domain-rect-y-max domain-rect) (/ approx-length 2)))
        (x-from (+ (domain-rect-x-min domain-rect) (/ approx-length 2)))
        (x-to   (- (domain-rect-x-max domain-rect) (/ approx-length 2))))
    (flatten 
     (loop for y from y-from to y-to by approx-length
           collect (loop for x from x-from to x-to by approx-length
                         collect (vec3 y x 1))))))



(defun fill-shape-domain-by-approx-loading-points (shape)
  (remove-if
   #'(lambda (point) (not (point-inner-domnain-p point shape)))
   (approx-points-list (shape-domain-rect shape))
  ))


;;;; synth

(defun synth (p1 p2)
  nil)


;;(defun synthesize ()
;;)





;;;; search solution



