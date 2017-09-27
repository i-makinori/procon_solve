(in-package #:procon-2016)



;;;; main, binary


(defun main ()
  (print (synthesize-piece *test-condi7* *test-condi8*))
  )

(defun make-binary ()
  (sb-ext:save-lisp-and-die "procon"
                            :toplevel #'main
                            :compression t
                            :executable t))
