(in-package #:procon)


;;;; values ;;;;;;;;;;;;;;;;;;;;;;
(defparameter *huge-num* 1000000)
(defparameter *-huge-num* -1000000)    


;;;; number-pair ;;;;;;;;;;;;;

(defun number-pair (list &optional (initial-value 0))
  (let ((val initial-value))
    (mapcar #'(lambda (x)
                (setq val (1+ val))
                (cons (- val 1) x))
            list)))


;;;; list ;;;;;;;;;;;;;;;;;;;

(defun init (list &optional searched-list)
  (if (cdr list)
      (init (cdr list) (cons (car list) searched-list))
      (reverse searched-list)))
  

(defun rotate-list (list &optional (rotate-times 1))
  (let ((n (mod rotate-times (length list))))
    (append (drop list n) (take list n))
  ))
  
(defun take (ls n)
  (if (or (<= n 0) (null ls))
      nil
    (cons (car ls) (take (cdr ls) (1- n)))))


(defun drop (ls n)
  (if (or (<= n 0) (null ls))
      ls
    (drop (cdr ls) (1- n))))

(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

(defun rotate-nth (n list &optional (length nil))
  (nth (mod n (if length length (length list)))
      list))

(defun append-car-last (list)
  ">> f '(1 2 3 4 5) -> (1 2 3 4 5 1)"
  (append list (list (car list))))

(defun drop-while (pred xs)
  (if (or (null xs) (not (funcall pred (car xs))))
      xs
      (drop-while pred (cdr xs))))

(defun drop-not-while (pred xs)
  (if (or (null xs) (funcall pred (car xs)))
      xs
      (drop-not-while pred (cdr xs))))

(defun safety-sort (list predicate)
  (cond ((null list) '())
        (t (let ((car-list (car list)))
             (append
              (safety-sort
               (remove-if #'(lambda (x) (funcall predicate car-list x)) (cdr list))
               predicate)
              (list (car list))
              (safety-sort
               (remove-if-not #'(lambda (x) (funcall predicate car-list x)) (cdr list))
               predicate) )))))


(defun flatten (orig-list)
    (if (eql orig-list nil)
        nil
        (let ((elem (car orig-list)) (resto-list (cdr orig-list)))
            (if (listp elem)
                (append (flatten elem) (flatten resto-list))
                (append (cons elem nil) (flatten resto-list))))))

(defun take-while (list test)
  (and list (funcall test (car list))
       (cons (car list) (take-while (cdr list) test))))


;;;; macros ;;;;;;;;;;;;;;;;;;;;
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defun extend (lst len)
  (let ((res (subseq lst 0 (mod len (length lst)))))
    (dotimes (i (1+ (floor (/ len (length lst)))))
      (setq res (append lst res)))
    res))

(defun cons-list-n (list &optional (n 0) (s-lis '()))
  (cond ((null list) (reverse s-lis))
        (t (cons-list-n (cdr list) (1+ n)
                        (cons (cons n (car list)) s-lis)))))


;;; tuple

(defun map-tuple (fn num lst)
  (do ((rest lst (cdr rest))
       (len (length lst) (1- len))
       (acc nil))
      ((< len num) (nreverse acc))
    (push (apply fn (subseq rest 0 num))
          acc)))

(defmacro do-tuple (params lst &body body)
  (with-gensyms (rest len num)
    `(do ((,rest ,lst (cdr ,rest))
          (,len (length ,lst) (1- ,len))
          (,num (length ',params)))
         ((< ,len ,num))
       (destructuring-bind ,params (subseq ,rest 0 ,num)
         ,@body))))


(defun map-tuple/c (fn num lst)
  (let (acc)
    (do* ((rest (extend lst (1- num)) (cdr rest))
          (len (length rest) (1- len)))
         ((< len num))
      (push (apply fn (subseq rest 0 num))
            acc))
    (nreverse acc)))

(defmacro do-tuple/c (params lst &body body)
  (with-gensyms (num rest len)
    `(do* ((,num (length ',params))
           (,rest (extend ,lst (1- ,num))
                  (cdr ,rest))
           (,len (length ,rest) (1- ,len)))
          ((< ,len ,num))
       (destructuring-bind ,params (subseq ,rest 0 ,num)
         ,@body))))

