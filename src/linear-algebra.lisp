
(in-package :puzzle-1617)

;;;; liner algebra


;;; types and optimizations

;;(deftype vec3 () '(vector float 3))
(deftype vec3 () '(vector float 3))

;; (typep '#(1.0 2.1 3.4) 'vec3) ;; test

;;  (declare (optimize (speed 3) (debug 0) (safety 0)))
;;  (declare (type (simple-array single-float (* *)) ma mb))


;;; defines

(defun vec3 (&optional (v0 0) (v1 0) (v2 1))
  ;;(declare (ftype (function (number number number) vec3) vec3-x))
  (declare (ftype (function (float float float) vec3) vec3-x))
  (make-array 3 :initial-contents `(,v0 ,v1 ,v2)))

(defun point (x y)
  (vec3 x y 1))

(defun vec3-x (vec3)
  (declare (ftype (function (vec3) float) vec3-x))
  (aref vec3 0))

(defun vec3-y (vec3)
  (declare (ftype (function (vec3) float) vec3-y))
  (aref vec3 1))

(defun vec3-j (vec3)
  (declare (ftype (function (vec3) float) vec3-j))
  ;; we use j. not z because z meanse height dimention.
  (aref vec3 2))

(defun matrix3x3
    (&optional (a00 0) (a01 0) (a02 0) (a10 0) (a11 0) (a12 0) (a20 0) (a21 0) (a22 0))
  (make-array '(3 3) :initial-contents `((,a00 ,a01 ,a02)
                                         (,a10 ,a11 ,a12)
                                         (,a20 ,a21 ,a22))))

(defparameter *zero-matrix-3x3*
  (matrix3x3 0 0 0 0 0 0 0 0 0))

(defparameter *identity-matrix-3x3*
  (matrix3x3 1 0 0 0 1 0 0 0 1))

(defun vec3-ser= (V1 V2)
  (and (ser= (vec3-x V1) (vec3-x V2))
       (ser= (vec3-y V1) (vec3-y V2))
       (ser= (vec3-j V1) (vec3-j V2))))


(defun vec3-multiply (V1 V2)
  (let ((V3 (vec3)))
    (loop for i from 0 to (- 3 1)
          do (setf (aref V3 i) (* (aref V1 i) (aref V2 i))))
    V3))


(defun vec3-add-xy (V1 V2)
  (vec3 (+ (vec3-x V1) (vec3-x V2))
        (+ (vec3-y V1) (vec3-y V2))
        1))

(defun vec3-sub-xy (V1 V2)
  (vec3 (- (vec3-x V1) (vec3-x V2))
        (- (vec3-y V1) (vec3-y V2))
        1))

(defun vec3-factor-xy (scalar Vn)
  (vec3 (* scalar (vec3-x Vn))
        (* scalar (vec3-y Vn))
        1))


(defparameter *vec3-zero-xy* #(0 0 1))

(defun vec3-inverse-xy (Vn)
  (vec3 (- (vec3-x Vn)) (- (vec3-y Vn)) 1))

(defun vec3-length-xy (Vn)
  (sqrt (vec3-length-xy^2 Vn)))

(defun vec3-length-xy^2 (Vn)
  (vec3-dot-xy Vn Vn))

(defun vec3-normalize-xy (Vn)
  (let ((length (vec3-length-xy Vn)))
    (if (< length *standard-error*)
        #(0 0 0)
        (vec3 (/ (vec3-x Vn) length) (/ (Vec3-y Vn) length) 1))))

(defun vec3-dot-xy (V1 V2)
  ;;(declare (optimize (speed 2) (debug 0) (safety 0)))
  (declare (ftype (function (vec3 vec3) float) vec3-dot-xy))
  (+ (* (vec3-x V1) (vec3-x V2))
     (* (vec3-y V1) (vec3-y V2))))

(defun vec3-dot (V1 V2)
  ;; (reduce #'+ (vec3-multiply V1 V2)) ;; <- slower implement
  (declare (ftype (function (vec3 vec3) float) vec3-dot))
  (+ (* (vec3-x V1) (vec3-x V2))
     (* (vec3-y V1) (vec3-y V2))
     (* (vec3-j V1) (vec3-j V2))))

(defun vec3-cross-xy (V1 V2)
  (declare (ftype (function (vec3 vec3) float) vec3-cross-xy))
  (- (* (vec3-x V1) (vec3-y V2))
     (* (vec3-y V1) (vec3-x V2))))

(defun vec3-cross (V1 V2)
  (declare (ftype (function (vec3 vec3) vec3) vec3-cross))
  (vec3 (- (* (vec3-y V1) (vec3-j V2)) (* (vec3-j V1) (vec3-y V2)))
        (- (* (vec3-j V1) (vec3-x V2)) (* (vec3-x V1) (vec3-j V2)))
        (- (* (vec3-x V1) (vec3-y V2)) (* (vec3-y V1) (vec3-x V2)))))

(defun vec3-matrix-row (An row)
  (vec3 (aref An row 0)
        (aref An row 1)
        (aref An row 2)))

(defun vec3-matrix-col (An col)
  (vec3 (aref An 0 col)
        (aref An 1 col)
        (aref An 2 col)))

(defun matrix3x3-vector3-product (An Vn)
  "An * Vn. where Vn as col vector."
  (vec3 (vec3-dot (vec3-matrix-row An 0) Vn)
        (vec3-dot (vec3-matrix-row An 1) Vn)
        (vec3-dot (vec3-matrix-row An 2) Vn)))

(defun matrix3x3-product (A1 A2)
  (let ((A3 (matrix3x3)))
  (loop for i from 0 to (- 3 1)
        do (loop for j from 0 to (- 3 1)
                 do (setf (aref A3 i j)
                          (vec3-dot (vec3-matrix-row A1 i)
                                    (vec3-matrix-col A2 j)))))
    A3))

;; reference of transformation matrix.
;; https://ch.mathworks.com/help/images/matrix-representation-of-geometric-transformations.html

#|
(defun transform-parallel-move (transform-x transform-y)
  (matrix3x3 1 0 transform-x
             0 1 transform-y
             0 0 1))
|#

(defun transform-parallel-move (vec-moves)
  (matrix3x3 1 0 (vec3-x vec-moves)
             0 1 (vec3-y vec-moves)
             0 0 1))

(defun transform-rotate (theta)
  "theta counts from x axis. at the +y axis."
  (matrix3x3 (+ (cos theta)) (- (sin theta)) 0
             (+ (sin theta)) (+ (cos theta)) 0
             0               0               1))

(defun transform-mirror-axis-x-axis (&optional (mirror -1))
  "mirror from x-axis."
  (let ((mirror-param (if (= mirror -1) -1 +1)))
    (matrix3x3 1 0            0
               0 mirror-param 0
               0 0            1)))

(defun transform-mirror-by-axis-vec (axis-vec)
  "mirror transform panels from axis-vec which origins(starts) from origin point."
  ;; Ref: https://ja.wikipedia.org/wiki/%E9%8F%A1%E6%98%A0
  ;; where mirrors point, not axis.
  (let* ((length^2 (vec3-length-xy^2 axis-vec))
         (length^2 (if (zerop length^2) ;; 0-direction. avoid (x/0).
                       (progn (warn (format nil "direction of ~A is not decidable~%" axis-vec))
                              1)
                       length^2))
         (ax (vec3-x axis-vec))
         (ay (vec3-y axis-vec)))
    (labels ((Rij (kro-delta Ai Aj)
               (- (- kro-delta (/ (* 2 Ai Aj) length^2))))) ;; mirrors points
      (matrix3x3 (Rij 1 ax ax) (Rij 0 ax ay) 0
                 (Rij 0 ay ax) (Rij 1 ay ay) 0
                 0             0             1))))



