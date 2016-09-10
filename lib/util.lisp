(in-package #:procon)


;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; functions ;;;;;;;;;;;;;;;;

(defun nearest (number)
  (values (floor (+ number 0.5))))

(defun a= (num1 num2)
  "approximately-equal"
  ;;digit : which digit for rounded?
  ;;<ex> digit =5, x.xxxA, round at A "
  (or (= (nearest num1) (nearest num2))
      (= (floor num2) (floor num1))))


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
