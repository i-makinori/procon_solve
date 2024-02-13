
(in-package :puzzle-1617)


;;;; utility

;; assoc

(defun assocdr (item alist)
  (cdr (assoc item alist))) 

;; mod-nth

(defun modnth (n list)
  (nth (mod n (length list)) list))
               


;; constant

(defparameter *standard-error* 0.0001)
(defparameter *pi* 3.141592653589793238462643)
(defparameter *pi/2* (/ *pi* 2))
(defparameter *pi*2* (* *pi* 2))

;; flat list
(defun flatten (ons-list-list)
  (apply #'concatenate 'list ons-list-list))

;; determine specific character
(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

;; split string
(defun split-string (string &optional (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
      :while end))


;; standard error


(defun ser= (val1 val2 &optional (standard-error *standard-error*))
  "standard error rounded equal"
  (< (abs (- val2 val1)) standard-error))


;; cycled list into tuple list

(defun make-tuple-list-aux (cycle-series cycle-car)
  (cond ((null (cdr cycle-series)) 
         (list (cons (car cycle-series) cycle-car)))
        (t
         (cons (cons (car cycle-series) (cadr cycle-series))
               (make-tuple-list-aux (cdr cycle-series)
                                    cycle-car)))))

(defun make-tuple-list (cycle-series)
  (make-tuple-list-aux cycle-series (car cycle-series)))

(defun map-tuple (function cycle-series)
  (mapcar #'(lambda (tuple) (funcall function (car tuple) (cdr tuple)))
          (make-tuple-list cycle-series)))
