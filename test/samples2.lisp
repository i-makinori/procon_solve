(in-package #:procon)


;;;; test

(defparameter 
    *test-piece-for-synth1*
  (spots->piece
   (mapcar #'cons->spot
           '((0 . 0) (-8 . 16) (0 . 8) (4 . 16)
             (8 . 16) (12 . 24) (16 . 16) (16 . 0)))))

(defparameter 
    *test-piece-for-synth2*
  (spots->piece
   (mapcar #'cons->spot
           '((8 . 8) (0 . 24) (32 . 24) (32 . 8) 
             (28 . 16) (24 . 8) (20 . 8) (16 . 16)))))

(defparameter *test-piece-list2*
  (list *test-piece-for-synth1* *test-piece-for-synth2*))
