
(in-package :puzzle-1617)


;;;; utility

;; determine specific character
(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

;; split string
(defun split-string (string &optional (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
      :while end))




;; cycled list into tuple list
(defun make-tuple-aux (cycle-series cycle-car)
  (cond ((null (cdr cycle-series)) 
         (list (cons (car cycle-series) cycle-car)))
        (t
         (cons (cons (car cycle-series) (cadr cycle-series))
               (make-tuple-aux (cdr cycle-series)
                           cycle-car)))))

(defun map-tuple (function cycle-series)
  (mapcar #'(lambda (tuple) (funcall function (car tuple) (cdr tuple)))
          (make-tuple-aux cycle-series (car cycle-series))))
