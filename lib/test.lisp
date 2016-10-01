(in-package #:procon)

;;;; test

(defun random-vecs (&optional (num 3))
  (mapcar #'(lambda (x) x
                    (cons (random 20) (random 20)))
          (make-list num)))

(defparameter *test-piece1*
  '((10 . 10) (10 . 40) (40 . 10)))

(defparameter *test-piece2*
  '((30 . 10) (10 . 40) (30 . 20) (50 . 40)))

(defparameter *test-piece3*
  '((0 . 0) (20 . 20) (20 . 0)))

(defparameter *test-piece4*
  '((0 . 0) (0 . 20) (20 . 20) (20 . 0)))

(defparameter *test-piece5*
  '((0 . 0) (20 . 10) (20 . 0)))

(defparameter *test-piece6*
  '((0 . 0) (20 . 0) (40 . 20) (0 . 20)))

(defparameter *test-piece7*
  '((0 . 0) (5 . -10) (5 . -30) (-15 . -30) (-25 . -10)
    (-15 . -20) (-10 . -10) (-5 . -10)))

(defparameter *test-piece8*
  '((0 . 0) (5 . -10) (5 . 10) (-35 . 10) (-25 . -10)
    (-15 . 0) (-10 . -10) (-5 . -10)))

(defparameter *test-piece9*
  '((0 . 0) (10 . 0) (10 . -20) (60 . -20) (60 . 0) (70 . 0)
    (70 . -30) (0 . -30)))

(defparameter *test-piece10*
  '((0 . 0) (10 . 0) (20 . 10) (10 . 20) (0 . 20) (0 . 10) (-10 . 10)
    (-10 . 30) (30 . 30) (30 . -10) (-10 . -10) (-10 . 10) (0 . 10)))

(defparameter *test-piece11*
  '((0 . 0) (10 . 0)(10 . 20) (0 . 20)))

(defparameter *test-piece12*
  '((0 . 0) (5 . 0) (10 . -5) (15 . 0) (10 . 5)
    (5 . 0) (0 . 0) (10 . -10) (20 . 0) (10 . 10)))

(defparameter *test-frame1*
  '((0 . 0) (60 . 0) (60 . 40) (0 . 40) (0 . 0)
    (10 . 10) (30 . 10) (50 . 30) (10 . 30)))


(defparameter *test-pieces*
  (list *test-piece1* *test-piece2* *test-piece3*
        *test-piece4* *test-piece5* *test-piece6*))

(defparameter *test-condi1*
  (make-piece-condition
   :piece (coord-to-piece *test-piece1*)
   :point 0
   :angle (/ PI 4)))

(defparameter *test-condi2*
  (make-piece-condition
   :piece (coord-to-piece *test-piece2*)
   :point 2
   :angle 0))
  
(defparameter *test-condi3*
  (make-piece-condition 
   :piece (coord-to-piece *test-piece3*)
   :two-side nil
   :point 2
   :angle (/ PI 1)))

(defparameter *test-condi4*
  (make-piece-condition
   :piece (coord-to-piece *test-piece4*)
   :two-side t
   :point 1
   :angle (/ PI 4)))

(defparameter *test-condi5*
  (make-piece-condition
   :piece (coord-to-piece *test-piece5*)
   :two-side nil
   :point 2
   :angle (/ PI -2)))

(defparameter *test-condi6*
  (make-piece-condition
   :piece (coord-to-piece *test-piece6*)
   :two-side nil
   :point 2
   :angle (/ pi 2)))

(defparameter *test-condi8*
  (make-piece-condition
   :piece (coord-to-piece *test-piece8*)
   :two-side t
   :point 0
   :angle 0))

(defparameter *test-condi7*
  (make-piece-condition
   :piece (coord-to-piece *test-piece7*)
   :two-side t
   :point 0
   :angle 0))

(defparameter *test-condi9-1*
  (make-piece-condition
   :piece (coord-to-piece *test-piece9*)
   :two-side t
   :point 0
   :angle 0))

(defparameter *test-condi9-2*
  (make-piece-condition
   :piece (coord-to-piece *test-piece9*)
   :two-side nil
   :point 0
   :angle pi))

(defparameter *test-condi10*
  (make-piece-condition
   :piece (coord-to-piece *test-piece10*)
   :two-side t
   :point 0
   :angle 0))

(defparameter *test-condi11*
  (make-piece-condition
   :piece (coord-to-piece *test-piece11*)
   :two-side t
   :point 0
   :angle 0))

(defparameter *test-piece12*
  '((0 . 0) (10 . 10) (0 . 20)))

(defparameter *test-condi12*
  (make-piece-condition
   :piece (coord-to-piece *test-piece12*)
   :two-side t
   :point 0
   :angle 00))


;;;; functions

(defun test-synth ()
  (print (synthesize-piece
          (print 
           (make-piece-condition 
            :piece (synthesize-piece *test-condi10* *test-condi11*)
            :point 13
            :two-side T))
          *test-condi12*)
))
    
