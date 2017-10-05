(in-package #:procon)


;;;; probrems by files / also test for io


(defparameter *test-piece-file4* "test/assets/puzzle_4.txt")
(defparameter *test-piece-file5* "test/assets/puzzle_5.txt")
(defparameter *test-piece-file6* "test/assets/puzzle_6.txt")
(defparameter *test-piece-file7* "test/assets/puzzle_7.txt")
(defparameter *test-piece-file8* "test/assets/puzzle_8.txt")
(defparameter *test-piece-file9* "test/assets/puzzle_9.txt")
(defparameter *test-piece-file10* "test/assets/puzzle_10.txt")


;;;; test for synth


;;;; test for gui


(defparameter *test-gui-piece-list*
  (piece-list->gui-piece-list *test-piece-list1*))

(defvar *solve-gui-state* nil)

(defun run-solve-gui-test ()
  (setf *solve-gui-state* 
        (make-application-frame 'solve-gui
                                :gui-piece-list *test-gui-piece-list*))
  (run-frame-top-level *solve-gui-state*))


