(in-package #:procon)



;;;; main, binary


(defun main ())

(defun make-binary ()
  (sb-ext:save-lisp-and-die "procon"
                            :toplevel #'main
                            :compression t
                            :executable t))
