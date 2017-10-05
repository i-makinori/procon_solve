(in-package :procon)

;;;; samples ;;;;;;;;;;;;;;;;;

;;;; generate test by json-file
;; for example,
;; REPL > 
#|
(defun piece-list->piece-list-func-form (piece-list)
  (mapcar 
   #'(lambda (spot-list)
       (format t "(spots->piece  (list ~{(spot~{ ~A~}) ~}))~%"
               (mapcar #'(lambda (vec)
                           (list (vx vec) (vy vec)))
                       spot-list)))
   (mapcar #'piece-spots piece-list)))
|#


;;;; test 

