
(in-package :puzzle-1617)

;;;; examples

;;; sample problem
(defparameter *example-shape1*
  (list #(0 0 1) #(18 0 1) #(16 8 1) #(18 18 1) #(9 18 1) #(9 17 1) #(2 16 1)
        #(2 18 1) #(0 18 1) #(0 14 1) #(2 11 1) #(1 8 1) #(3 3 1) #(0 2 1)))

(defparameter *example-problem-9*
  (load-problem-file-into-puzzle "puzzle_9.txt"))

(defparameter *example-problem-10*
  (load-problem-file-into-puzzle "puzzle_10.txt"))


#|
(defparameter *piece-hole1*
  ((lambda (p_dush)
     (setf (shape-pm-sign (piece-shape p_dush)) *-shape*)
     p_dush)
   (copy-piece (nth 1 *example-problem-10*))))
|#


