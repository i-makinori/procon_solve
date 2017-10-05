(in-package :procon)

;;;; samples ;;;;;;;;;;;;;;;;;

(defun piece-list->piece-list-func-form (piece-list)
  "(PIECE-LIST->PIECE-LIST-FUNC-FORM (coordinate-text->piece-list *test-piece-file9*))"
  (mapcar 
   #'(lambda (vecs-list)
       (format t "(piece  (list ~{(vec~{ ~A~}) ~}))~%"
               (mapcar #'(lambda (vec)
                           (list (vx vec) (vy vec)))
                       vecs-list)))
   (mapcar #'piece-vecs piece-list)))


;;;; test 

(defparameter *test-piece-list1*
  (list (piece  (list (vec 0 0) (vec 0 -18) (vec 18 -18) (vec 18 0) ))
        (piece  (list (vec 0 0) (vec -8 2) (vec -6 -6) (vec 0 -14) ))
        (piece  (list (vec 0 0) (vec 1 4) (vec 8 8) (vec 14 0) ))
        (piece  (list (vec 0 0) (vec 4 0) (vec 5 4) (vec 4 14) (vec 0 15) ))
        (piece  (list (vec 0 0) (vec -7 -4) (vec -8 6) (vec -2 8) ))
        (piece  (list (vec 0 0) (vec 0 -3) (vec 4 -4) (vec 10 -2) (vec 18 -4) (vec 18 0) ))))


(defparameter *test-piece1* 
  (nth 0 *test-piece-list1*))

(defparameter *test-piece2*
  (nth 1 *test-piece-list1*))
