(in-package #:procon)


(defun rad-to-360 (rad)
  (* 360 (/ rad pi)))

(defun 360-to-rad (angle)
  (* pi (/ angle 360)))

(defparameter *a-deg* (/ (* PI 2) 360)
  "amount-degree standard-coefficient (standard-error)")

(defparameter *a* 2)

(defparameter *a-angle-round* (360-to-rad 15))

(defun a= (num1 num2)
  (and (< (- num1 *a*) num2)
       (> (+ num1 *a*) num2)))


;;;vector


(defun a-vec= (vec1 vec2)
  (and (a= (vx vec1) (vx vec2))
       (a= (vy vec1) (vy vec2))))


;;; angle 
(defun a-d= (deg1 deg2 &optional (a-deg 1))
  "standard-error -degrees-refrected-equal"
  (and (< deg1 (+ deg2 (* a-deg *a-deg*)))
       (> deg1 (- deg2 (* a-deg *a-deg*)))))


(defun a-2pi-judge (deg)
  (> deg (- *2pi* *a-angle-round*)))

(defun a-0pi-judge (deg)
  (> deg *a-angle-round*))

