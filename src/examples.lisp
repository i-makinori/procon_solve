
(in-package :puzzle-1617)

;;;; examples

;;; sample problem
(defparameter *example-shape1*
  (list #(0 0 1) #(18 0 1) #(16 8 1) #(18 18 1) #(9 18 1) #(9 17 1) #(2 16 1)
        #(2 18 1) #(0 18 1) #(0 14 1) #(2 11 1) #(1 8 1) #(3 3 1) #(0 2 1)))

(defparameter *example-problem-10*
  (load-problem-file-into-puzzle "puzzle_10.txt"))
