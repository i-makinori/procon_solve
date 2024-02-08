
(in-package :puzzle-1617)

;; liner algebra

(defun vec3 (&optional (v0 0) (v1 0) (v2 0))
  (make-array 3 :initial-contents `(,v0 ,v1 ,v2)))

(defun vec3-x (vec3)
  (aref vec3 0))

(defun vec3-y (vec3)
  (aref vec3 1))

(defun vec3-j (vec3)
  ;; we use j. not z because z meanse height dimention.
  (aref vec3 2))

(defun matrix3x3
    (&optional (a00 0) (a01 0) (a02 0) (a10 0) (a11 0) (a12 0) (a20 0) (a21 0) (a22 0))
  (make-array '(3 3) :initial-contents `((,a00 ,a01 ,a02)
                                         (,a10 ,a11 ,a12)
                                         (,a20 ,a21 ,a22))))

(defun vec3-multiply (V1 V2)
  (let ((V3 (vec3)))
    (loop for i from 0 to (- 3 1)
          do (setf (aref V3 i) (* (aref V1 i) (aref V2 i))))
    V3))

(defun vec3-dot (V1 V2)
  (reduce #'+ (vec3-multiply V1 V2)))

(defun vec3-matrix-row (An row)
  (let ((Vn (vec3)))
    (loop for iter from 0 to (- 3 1)
          do (setf (aref Vn iter) (aref An row iter)))
    Vn))

(defun vec3-matrix-col (An col)
  (let ((Vn (vec3)))
    (loop for iter from 0 to (- 3 1)
          do (setf (aref Vn iter) (aref An iter col)))
    Vn))

(defun matrix3x3-vector3-product (An Vn)
  "An * Vn. where Vn as col vector."
  (let ((V1 (vec3)))
    (loop for iter from 0 to (- 3 1)
          do (setf (aref V1 iter) (vec3-dot (vec3-matrix-row An iter) Vn)))
    V1))


(defun product-matrix3x3 (A1 A2)
  (let ((A3 (transformation-matrix)))
  (loop for i from 0 to (- 3 1)
        do (loop for j from 0 to (- 3 1)
                 do (setf (aref A3 i j)
                          (vec3-dot (vec3-matrix-row A1 i)
                                    (vec3-matrix-col A2 j)))))
    A3))

;; reference of transformation matrix.
;; https://ch.mathworks.com/help/images/matrix-representation-of-geometric-transformations.html

(defun transform-parallel-move (transform-x transform-y)
  (matrix3x3 1 0 transform-x
             0 1 transform-y
             0 0 1))

(defun transform-rotate (theta)
  "theta counts from x axis. at the +y axis, theta is (1/2)*pi ."
  (matrix3x3 (cos theta) (sin theta) 0
             (sin theta) (cos theta) 0
             0           0           1))

(defun transform-mirror-axis-x-axis ()
  "mirror from x-axis. theta = 0."
  (matrix3x3 1 0  0
             0 -1 0
             0 0  1))
