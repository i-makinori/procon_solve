(in-package #:procon)

;;;; util

(defun rad-to-360 (rad)
  (* 180 rad (/ pi)))

(defun 360-to-rad (angle)
  (* pi (/ angle 360)))

(defmacro def-a= (name error-val)
  `(defun ,name (num1 num2)
     (and (< (- num1 ,error-val) num2)
          (> (+ num1 ,error-val) num2))))

;;;; number 
(defparameter *a-float* 0.001)

(defparameter *a* 50)
#|(defparameter *a/2* (/ *a* 2))
(defparameter *a/3* (/ *a* 3))
(defparameter *a/4* (/ *a* 4))
(defparameter *a2* (* *a* 2) "2 * *A*")
|#
(defparameter *a/2* *a* )
(defparameter *a/3* *a* )
(defparameter *a/4*  *a*)
(defparameter *a2*  *a* )

(defparameter *a-angle-round* (360-to-rad 15))

(def-a= a= *a*)
(def-a= a/2= *a/2*)
(def-a= a2= *a2*)


;;;vector


(defun a-vec= (vec1 vec2)
  (and (a= (vx vec1) (vx vec2))
       (a= (vy vec1) (vy vec2))))


;;; angle

(defparameter *a-deg* (360-to-rad 15)
  "amount-degree standard-coefficient (standard-error)")
(defparameter *a10-deg* *a-deg*)
(defparameter *a5-deg* *a-deg*)
#|
(defparameter *a10-deg* (* *a-deg* 10))
(defparameter *a5-deg* (* *a-deg* 5))
|#

(def-a= a-d= *a-deg*)
(def-a= a-10d= *a10-deg*)
(def-a= a-5d= *a5-deg*)

(defun a-0pi-judge (angle)
  (a-10d= angle 0) )

(defun a-pi-judge (angle)
  (a-5d= angle pi)  )

(defun a-2pi-judge (angle)
  (a-5d= angle *2pi*)
  )

