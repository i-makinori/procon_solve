
(in-package :puzzle-1617)


;;;; utility

;; list
(defun first-n (n lst)
  (subseq lst 0 (min (length lst) n)))

(defun insert (thing into &key (older-function #'<))
  "insert thing into list.
 if you use insert for the as sort, into need to be sorted"
  (labels ((aux (rest memo)
             (cond  ((or (null rest)
                         (funcall older-function thing (car rest)))
                     (append (reverse (cons thing memo)) rest))
                    (t (aux (cdr rest) (cons (car rest) memo))))))
    (aux into '())))

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

(defparameter *num-divergent* 999999)
(defparameter *num-zero* 0)

;; list, number collection from m to n.
(defun from-m-to-n-list (from-m to-n)
  (loop for i from from-m to to-n
        collect i))

;; flat list
(defun flatten (ons-list-list)
  (apply #'concatenate 'list ons-list-list))

(defun flat-2d-nest-list (list-list-list)
  (apply #'append (apply #'append  list-list-list)))

;; rot list (rotated list, modular)
(defun rot-left (n l)
  (append (nthcdr n l) (butlast l (- (length l) n))))

(defun rot-right (n l)
  (rot-left (- (length l) n) l))


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
  "standard error rounded equal (=)"
  (< (abs (- val2 val1)) standard-error))

(defun ser> (val1 val2 &optional (standard-error *standard-error*))
  "standard error rounded greater-than (>)"
  (and (> val1 val2)
       (not (ser= val1 val2 standard-error))))

(defun ser< (val1 val2 &optional (standard-error *standard-error*))
  "standard error rounded less-than (<)"
  (and (< val1 val2)
       (not (ser= val1 val2 standard-error))))


(defun ser>= (val1 val2 &optional (standard-error *standard-error*))
  "standard error rounded greater-or-equal-than (>=)"
  (or  (>= val1 val2)
       (ser= val1 val2 standard-error)))

(defun ser<= (val1 val2 &optional (standard-error *standard-error*))
  "standard error rounded less-or-equal-than (=<)"
  (or  (<= val1 val2)
       (ser= val1 val2 standard-error)))


;; cycled list into tuple list

(defun make-tuple-list-aux (cycle-series cycle-car)
  (cond ((null cycle-series) nil)
        ((null (cdr cycle-series))
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


;; 3tuple list

(defun make-3tuple-list-aux (cycle-series fst snd)
  (cond ((null (identity cycle-series))
         nil)
        ((null (cdr cycle-series))
         (cons (cons (car cycle-series) (cons fst snd))
               (make-3tuple-list-aux (cdr cycle-series) fst snd)))
        ((null (cddr cycle-series))
         (cons (cons (car cycle-series) (cons (cadr cycle-series) fst))
               (make-3tuple-list-aux (cdr cycle-series) fst snd)))
        (t
         (cons (cons (car cycle-series) (cons (cadr cycle-series) (caddr cycle-series)))
               (make-3tuple-list-aux (cdr cycle-series) fst snd)))))

(defun make-3tuple-list (cycle-series)
  (make-3tuple-list-aux cycle-series
                        (car cycle-series)
                        (if (null (cadr cycle-series))
                            (car cycle-series)
                            (cadr cycle-series))))

;; set theory


(defun cartesian (&rest lists)
  ;; Ref: https://gist.github.com/capablemonkey/4133438ba7043af94691a2b54d997e8b
  (labels ((cartesian2 (a b)
             (mapcan (lambda (item-from-a)
                       (mapcar (lambda (item-from-b)
                                 (if (listp item-from-a)
                                     (append item-from-a (list item-from-b))
                                     (list item-from-a item-from-b)))
                               b))
                     a)))
    (reduce #'cartesian2 lists)))

(defun set-equal (set-list1 set-list2 &key (test #'eql))
  (and (subsetp set-list1 set-list2 :test test)
       (subsetp set-list2 set-list1 :test test)))


(defun remove-equally-set-from-set-list (set-list &key (test #'eql))
  (labels ((aux (lis memo)
             (cond ((null lis) memo)
                   (t (aux (remove-if #'(lambda (scdn) (set-equal (car lis) scdn :test test))
                                      (cdr lis))
                           (cons (car lis) memo))))))
    (reverse (aux set-list '()))))
