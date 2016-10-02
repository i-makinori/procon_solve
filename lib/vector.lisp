(in-package #:procon)

(declaim (inline vx vy point-to-vec vec-sub dxdy vec-add vec-prod vec-dot vector-to-point
                 vector-to-line ))

;;;; struct

(defmacro vec (dx dy)
  `(cons ,dx ,dy))

(defmacro deg (deg)
  `,deg)

(defmacro dimension (dimension)
  "length, how long is it"
  `,dimension)


;;;; defines

(defun dist (vec1 vec2)
  (sqrt (+ (expt (car (dxdy vec1 vec2)) 2)
           (expt (cdr (dxdy vec1 vec2)) 2))))

(defun vector-dist (vec)
  (sqrt (+ (expt (vx vec) 2)
           (expt (vy vec) 2))))

(defun vx (vec)
  (car vec))

(defun vy (vec)
  (cdr vec))

(defun point-to-vec (point)
  (vec (x1 point) (y1 point)))

(defun dxdy (vec1 vec2)
  (vec (- (vx vec2) (vx vec1))
       (- (vy vec2) (vy vec1))))

(defun vec-sub (vec1 vec2)
  "vector subtraction"
  (vec (- (vx vec1) (vx vec2))
       (- (vy vec1) (vy vec2))) )

(defun vec-add (vec1 vec2)
  (vec (+ (vx vec1) (vx vec2))
       (+ (vy vec1) (vy vec2))))

(defun vec-prod (vec1 vec2)
  "vector product"
  (vec (* (vx vec1) (vx vec2))
       (* (vy vec1) (vy vec2))))

(defun vec-dot (vec1 vec2)
  "vector inner product"
  (+ (* (vx vec1) (vx vec2))
     (* (vy vec1) (vy vec2))))

(defun vec-cross (vec1 vec2)
  "vector cross product"
  (- (* (vx vec1) (vy vec2))
     (* (vx vec2) (vy vec1))))

(defparameter *angle-vec-criteria*
  (vec 0 10))

(defparameter *vec-origin*
  (vec 0 0))

;;;; functions

(defun line-collision-detection (line1 line2)
  "line hit-judge"
  (let ((ax (x1 line1)) (ay (y1 line1))
        (bx (x2 line1)) (by (y2 line1))
        (cx (x1 line2)) (cy (y1 line2))
        (dx (x2 line2)) (dy (y2 line2)))
    (let ((ta (+ (* (- cx dx) (- ay cy)) (* (- cy dy) (- cx ax))))
          (tb (+ (* (- cx dx) (- by cy)) (* (- cy dy) (- cx bx))))
          (tc (+ (* (- ax bx) (- cy ay)) (* (- ay by) (- ax cx))))
          (td (+ (* (- ax bx) (- dy ay)) (* (- ay by) (- ax dx)))))
      (and (< (* tc td) 0)
           (< (* ta tb) 0)))))

(defun line-point-vec-hit-judge (line point-vec)
  (let ((l1 (sqrt (+ (square (- (x2 line) (x1 line)))
                     (square (- (y2 line) (y1 line))))))
        (l2 (sqrt (+ (square (- (vx point-vec) (x1 line)))
                     (square (- (vy point-vec) (y1 line)))))))
    (and (>= l1 l2)
         (= (* l1 l2)
            (+ (* (- (x2 line) (x1 line)) (- (vx point-vec) (x1 line)))
               (* (- (y2 line) (y1 line)) (- (vy point-vec) (y1 line))))))))

(defun lines-point-vec-hit-judge (line-list point-vec)
  (some
   #'(lambda (line)
       (line-point-vec-hit-judge line point-vec))
   line-list))

(defun same-vector-angle (vec1 vec2)
  (let ((angle (vectors-to-angle vec1 '(0 . 0) vec2)))
    (or (a-d= angle *2pi*)
        (a-d= angle 0))))

  
(defun gravity-center (vectors)
  (let ((length (length vectors)))
    (vec (/ (reduce #'+ (mapcar #'vx vectors)) length)
         (/ (reduce #'+ (mapcar #'vy vectors)) length))))

;; util

(defun vector-to-point (vector)
  (list (car vector) (cdr vector)))

(defun vector-to-line (vector-start vector-delta)
  (line (vx vector-start) (vy vector-start)
        (vx vector-delta) (vy vector-delta)))

(defun angle-vectors-adjust (vecs)
  "when vecs includes origin-vector, shift each vector-dxdy 10"
  (if (some #'(lambda (vec) (equal vec '(0 . 0)))
            vecs)
      (angle-vectors-adjust
       (mapcar #'(lambda (vec) (vec-add '(10 . 10) vec))
               vecs))
      vecs))

(defun vectors-to-angle (vec1 vec2 vec3)
  (let ((adjust-vecs (angle-vectors-adjust (list vec1 vec2 vec3))))
    (angle (vector-to-point (nth 0 adjust-vecs))
           (vector-to-point (nth 1 adjust-vecs))
           (vector-to-point (nth 2 adjust-vecs)))))

(defun vector-angle (vec)
  (vectors-to-angle *angle-vec-criteria* *vec-origin* vec))




;;;; standard-error

(defun line-collision-detection-error (line1 line2)
  "standard-error refrected  line hit-judge"
  (and (line-collision-detection line1 line2)
       ()
  ))

(defun line-point-vec-hit-judge-error (line point-vec)
  (let ((l1 (sqrt (+ (square (- (x2 line) (x1 line)))
                     (square (- (y2 line) (y1 line))))))
        (l2 (sqrt (+ (square (- (vx point-vec) (x1 line)))
                     (square (- (vy point-vec) (y1 line)))))))
    (and (>= l1 l2)
         (= (* l1 l2)
            (+ (* (- (x2 line) (x1 line)) (- (vx point-vec) (x1 line)))
               (* (- (y2 line) (y1 line)) (- (vy point-vec) (y1 line))))))))

(defun same-vector-angle-error (vec1 vec2)
  (let ((angle (vectors-to-angle vec1 '(0 . 0) vec2)))
    (or (a-d= angle *2pi*)
        (a-d= angle 0))))
