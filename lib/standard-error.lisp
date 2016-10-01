(in-package #:procon)

(defun nearest (number)
  (values (floor (+ number 0.5))))

(defun a= (num1 num2)
  #|"approximately-equal"
  ;;digit : which digit for rounded?
  ;;<ex> digit =5, x.xxxA, round at A "|#
  (or (= (nearest num1) (nearest num2))
      (= (floor num2) (floor num1))))

(defparameter *a* 1)

(defparameter *a-deg* (/ (* PI 2) 360)
  "amount-degree standard-coefficient (standard-error)")

(defun a-d= (deg1 deg2 &optional (a-deg 1))
  "standard-error -degrees-refrected-equal"
  (and (< deg1 (+ deg2 (* a-deg *a-deg*)))
       (> deg1 (- deg2 (* a-deg *a-deg*)))))

