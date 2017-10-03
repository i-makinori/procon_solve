(in-package :procon)

;;;; utils ;;;;;;;;;;;;;




;;;; hoge constant 

(defparameter *huge-num* 100000000)
(defparameter *-huge-num* (- *huge-num*))

;;;; number-pair 



;;;; degree

(defun rad->360 (rad)
  (* rad 180 (/ pi)))

(defun 360->rad (deg)
  (* deg pi (/ 180)))

(defparameter 2pi (* 2 pi))


;;;; error-rounding standard error 
(defparameter *error-round-length* 0.01)
(defparameter *error-round-deg* (rad->360 (* pi (/ 0.01  180))))


(defun error-round-length= (length1 length2)
  (and (< (- length1 *error-round-length*) length2)
       (> (+ length1 *error-round-length*) length2)))


(defun error-round-deg= (deg1 deg2)
  (and (< (- deg1 *error-round-deg*) deg2)
       (> (+ deg1 *error-round-deg*) deg2)))

(defun real-num->maybe-integer (real-num)
  "by standard error.  
if (error of real-num < *standard-error*) than Just (round real-num) else nothing(=fail)"
  (multiple-value-bind (int-num error-num) 
      (round real-num)
    (if (> *error-round-length* error-num)
        (just int-num)
        (nothing))))

(defun 2pi-judge-contain-error (its-about-2pi)
  (error-round-deg= 2pi its-about-2pi))

(defun pi-judge-contain-error (its-about-pi)
  (error-round-deg= pi its-about-pi))



              
;;;; abstruct function

(defun show-param (indent name var)
  (format t "~&~V@{  ~}~A :: ~A~%" indent name var)
  var)

(defun id (val)
  val)

(defun when-cons-car (test-var)
  (if (consp test-var) (car test-var) test-var))

(defun when-cons-cdr (test-var)
  (if (consp test-var) (cdr test-var) nil))



;;;; list 

(defun concat (list)
  (apply #'append list))

(defun upto (from to)
  "like haskell [from,from+1..num-1,num]"
  (declare (type integer from to))
  (if (<= from to)
      (cons from (upto (1+ from) to))))

(defun init (list &optional searched-list)
  (if (cdr list)
      (init (cdr list) (cons (car list) searched-list))
      (reverse searched-list)))

(defun rotate-list (list &optional (rotate-times 1))
  (let ((n (mod rotate-times (length list))))
    (append (drop list n) (take list n))
    ))

(defun number-pair (list &optional (initial-value 0))
  (let ((val initial-value))
    (mapcar #'(lambda (x)
                (setq val (1+ val))
                (cons (- val 1) x))
            list)))

(defun take (list num)
  (if (or (<= num 0) (null list))
      nil
      (cons (car list) (take (cdr list) (1- num)))))

(defun drop (list num)
  (if (or (<= num 0) (null list))
      list
      (drop (cdr list) (1- num))))

(defun remove-nth (num list)
  (declare
   (type integer num)
   (type list list))
  (if (or (zerop num) (null list))
      (cdr list)
      (cons (car list) (remove-nth (1- num) (cdr list)))))

(defun rotate-nth (n list)
  (let ((length (length list)))
    (nth (mod n (cond ((zerop length) 1)
                      (t length)))
         list)))

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
  (if (and list (funcall test (car list)))
      (cons (car list) (take-while (cdr list) test))))

(defun zip-list (list1 list2)
  " [a] -> [b] -> [(a, b)] "
  (concat (mapcar #'(lambda (v1) (mapcar #'(lambda (v2) (cons v1 v2)) list2))
                  list1)))

;;;; string-list 

(defun concat-string-list-list (string-list-list)
  (reduce 
   #'(lambda (s1 s2) (concatenate 'string s1 s2))
   string-list-list))


;;;; macros 
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

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))


;;;; rotate-list tuple

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

