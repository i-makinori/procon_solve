
(in-package :procon)

;;;; geometry ;;;;;;;;;;;


;;;; struct

(defstruct (piece-deg (:conc-name deg-))
  deg)

(defun deg (deg)
  "(degree::velue)->piece-deg"
  (make-piece-deg :deg deg))


(defstruct (piece-vec (:conc-name))
  vx vy)

(defun vec (dx dy)
  "make-piece-vec"
  (make-piece-vec :vx dx :vy dy))

;;;; convert

(defun spot->vec (spot)
  (vec (spot-x spot) (spot-y spot)))

(defun vec->spot (vec)
  (spot (vx vec) (vy vec)))

(defun spots->vecs (spot-list)
  (mapcar #'spot->vec spot-list))

(defun vecs->spots (vector-list)
  (mapcar #'vec->spot vector-list))

(defun vector-to-spot (vec)
  (spot (vx vec) (vy vec)))

(defun vector-to-line (vector-start vector-delta)
  (line (vx vector-start) (vy vector-start)
        (vx vector-delta) (vy vector-delta)))

;;;; functions

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


;;;; test
(defparameter *test-vec1* (vec 10 20))
(defparameter *test-vec2* (vec 60 80))
(defparameter *test-vec3* (vec -40 10))



;;;; functions

(defun dist (vec1 vec2)
  (sqrt (+ (expt (car (dxdy vec1 vec2)) 2)
           (expt (cdr (dxdy vec1 vec2)) 2))))

(defun vector-dist (vec)
  (sqrt (+ (expt (vx vec) 2)
           (expt (vy vec) 2))))


(defun line-collision-detection (line1 line2)
  "line hit-judge"
  (let ((ax (line-x1 line1)) (ay (line-y1 line1))
        (bx (line-x2 line1)) (by (line-y2 line1))
        (cx (line-x1 line2)) (cy (line-y1 line2))
        (dx (line-x2 line2)) (dy (line-y2 line2)))
    (let ((ta (+ (* (- cx dx) (- ay cy)) (* (- cy dy) (- cx ax))))
          (tb (+ (* (- cx dx) (- by cy)) (* (- cy dy) (- cx bx))))
          (tc (+ (* (- ax bx) (- cy ay)) (* (- ay by) (- ax cx))))
          (td (+ (* (- ax bx) (- dy ay)) (* (- ay by) (- ax dx)))))
      (and (< (* tc td) 0)
           (< (* ta tb) 0)))))

(defun line-spot-vec-hit-judge (line spot-vec)
  (let ((l1 (sqrt (+ (square (- (line-x2 line) (line-x1 line)))
                     (square (- (line-y2 line) (line-y1 line))))))
        (l2 (sqrt (+ (square (- (vx spot-vec) (line-x1 line)))
                     (square (- (vy spot-vec) (line-y1 line)))))))
    (and (>= l1 l2)
         (= (* l1 l2)
            (+ (* (- (line-x2 line) (line-x1 line)) (- (vx spot-vec) (line-x1 line)))
               (* (- (line-y2 line) (line-y1 line)) (- (vy spot-vec) (line-y1 line))))))))

(defun lines-spot-vec-hit-judge (line-list spot-vec)
  (some
   #'(lambda (line)
       (line-spot-vec-hit-judge line spot-vec))
   line-list))

(defun same-vector-angle (vec1 vec2)
  (let ((angle (vectors-to-angle vec1 '(0 . 0) vec2)))
    (or (error-round-deg= 2pi angle)
        (error-round-deg= 2pi angle))))


(defun gravity-center (vectors)
  (let ((length (length vectors)))
    (vec (/ (reduce #'+ (mapcar #'vx vectors)) length)
         (/ (reduce #'+ (mapcar #'vy vectors)) length))))

;;;; vector<->angle

(defun angle-vectors-adjust (vecs)
  "when vecs includes origin-vector, shift each vector-dxdy 10"
  (if (some #'(lambda (vec) (equal vec '(0 . 0)))
            vecs)
      (angle-vectors-adjust
       (mapcar #'(lambda (vec) (vec-add '(10 . 10) vec))
               vecs))
      vecs))

(defun vectors-to-angle (vec1 vec2 vec3)
  "ok"
  (let ((adjust-vecs (angle-vectors-adjust (list vec1 vec2 vec3))))
    (angle (vector-to-spot (nth 0 adjust-vecs))
           (vector-to-spot (nth 1 adjust-vecs))
           (vector-to-spot (nth 2 adjust-vecs)))))

(defun vector-angle (vec)
  (vectors-to-angle *angle-vec-criteria* *vec-origin* vec))

(defun clock-wise-angle (vec1 vec2 vec3) 
  "vec1 from to vec2"
  (let ((angle (vectors-to-angle vec1 vec2 vec3)))
    (if (> angle 0) angle
        (+ PI angle))))


;;;; standard-error

(defun line-spot-vec-hit-judge-error (line spot-vec)
  (let ((l1 (sqrt (+ (square (- (line-x2 line) (line-x1 line)))
                     (square (- (line-y2 line) (line-y1 line))))))
        (l2 (sqrt (+ (square (- (vx spot-vec) (line-x1 line)))
                     (square (- (vy spot-vec) (line-y1 line)))))))
    (and (>= l1 l2)
         (error-round-length=
          (* l1 l2)
          (+ (* (- (line-x2 line) (line-x1 line)) (- (vx spot-vec) (line-x1 line)))
             (* (- (line-y2 line) (line-y1 line)) (- (vy spot-vec) (line-y1 line))))))))

(defun line-collision-detection-error (line1 line2)
  "standard-error refrected  line hit-judge"
  (and (line-collision-detection line1 line2)
       (not (or (line-spot-vec-hit-judge-error line1 (vec (line-x1 line2) (line-y1 line2)))
                (line-spot-vec-hit-judge-error line1 (vec (line-x2 line2) (line-y2 line2)))
                (line-spot-vec-hit-judge-error line2 (vec (line-x1 line1) (line-y1 line1)))
                (line-spot-vec-hit-judge-error line2 (vec (line-x2 line1) (line-y2 line1)))))))
