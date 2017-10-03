(in-package #:procon)

;;;; maybe nothing is maybe nothing


;;;; Nothing 

(defun nothing () 
  "Maybe - Nothing(=Failure)"
  ;; f:failure
  '@nothing)

(defparameter *nothing* (nothing))

(defun failure ()
  (nothing))

(defun nothing-p (var)
  (eq var (nothing)))

(defun nothing=>nil (var)
  (if (eq var (nothing)) nil var))

(defun nil=>nothing (var)
  (if (null var) (nothing) var))

#|
(defmacro call-when-non-nothing ((func &body body))
  `(if (some #'is-nothing (list ,@body))
       (nothing)
       (,func ,@body)))
|#

(defmacro let-unless-body-bind-no-nothng (bindings &body body)
  "let form.  only when there is no-nothings in bindings, do ,@body, else (nothing)"
  (let ((bindings-names 
         `,(mapcar #'(lambda (bind) (car bind))
                   bindings)))
    `(let ,bindings
       (if (some #'nothing-p (list ,@bindings-names))
           (nothing)
           (progn ,@body)))))


;;;; Maybe

(defun just (a) 
  ;; t:sucess, true
  "Maybe - Just a"
  (cons '@just a))

(defun just-p (just-var)
  (and (consp just-var)
       (eq (car just-var) '@just)))

(defun maybe-p (a)
  (or (just-p a) 
      (nothing-p a)))

(define-condition maybe-data-type-error (simple-error) (data)
  (:report (lambda (c s)
             (format s "Maybe-data-type:difference : ~A" c))))

(defun maybe-return (maybe-a)
  (cond ((not (maybe-p maybe-a)) 
         (error 'maybe-data-type-error))
        ((just-p maybe-a) 
         (cdr maybe-a))
        (t (nothing))))

(defun just-*-nil=>nothng (var)
  "var => Maybe var (= Just var|Nothng). && (var==nil)=>Nothing"
  (cond (var (just var))
        (t (nothing))))

(defmacro let-maybe (bindings-of-maybe &body body)
  "let form.  if all bindings are just form, do ,@body, else (nothing)"
  (let ((bindings-names
         `,(mapcar #'(lambda (bind)  (car bind))
                   bindings-of-maybe))
        (bindings-funcs
         `,(mapcar #'(lambda (bind) `(maybe-return ,(cadr bind)))
                   bindings-of-maybe)))
    (let ((next-bindings 
           `,(mapcar #'list bindings-names bindings-funcs)))
      `(let
           ,next-bindings
         (if (some #'nothing-p (list ,@bindings-names))
             (nothing)
             (progn ,@body))))))

(defun list-of-maybe->maybe-list (list-of-maybe)
  "[Maybe a] -> Maybe [a] (= Just [a]||nothng)  
if (all just-p list) then (just list) else (nothing)."
  (let ((next-list (mapcar #'maybe-return list-of-maybe)))
    (if (find *nothing* next-list) 
        (nothing)
        (just next-list))))

(defun return-list-remove-nothing (list-of-maybe)
  "[Maybe a] -> [a]"
  (maybe-return (list-of-maybe->maybe-list
                 (remove *nothing* list-of-maybe))))



