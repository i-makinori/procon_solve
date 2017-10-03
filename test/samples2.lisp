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



;;;; test for synth

(defparameter *test-easy-piece1*
  (piece->easy-piece *test-piece1*))

(defparameter *test-easy-piece2*
  (piece->easy-piece (nth 2 *test-piece-list1*)))


(defparameter *test-synth1* 
  (synth *test-piece-for-synth1* *plus* 5))

(defparameter *test-synth2*
  (synth *test-piece-for-synth2* *minus* 4))


(defparameter *test-piece-n1*
  (spots->piece (list (spot 0 0) (spot 10 0) (spot 10 10) (spot 0 10))))

(defparameter *test-piece-n2*
  (let ((piece (spots->piece (list (spot 0 0) (spot 0 20) (spot 10 20) (spot 20 0)))))
    (piece (piece-spots piece)
           (piece-degrees piece)
           t nil nil)))

(defparameter *test-synth-n1*
  (synth *test-piece-n1* *plus* 1))

(defparameter *test-synth-n2*
  (synth *test-piece-n2* *minus* 0))


;;;; test for gui

(defparameter *test-gui-piece-list*
  (piece-list->gui-piece-list *test-piece-list1*))

(defvar *solve-gui-state* nil)

(defun run-solve-gui-test ()
  (setf *solve-gui-state* 
        (make-application-frame 'solve-gui
                                :gui-piece-list *test-gui-piece-list*))
  (run-frame-top-level *solve-gui-state*))

