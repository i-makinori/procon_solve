(eval-when (:execute :compile-toplevel :load-toplevel)
  (ql:quickload :puzzle-1617))


(in-package :puzzle-1617)
(use-package :cl)

;;

(defun help-formula-to-input ()
  )


;;

(defun call-solver-grad-beam (problem)
  (time (progn (search-solution-grad-beam problem)
               nil)))

;; main

(defun main (args)
  ;;(print args)
  (let ((arg-list (split-string args #'(lambda (c) (char= c #\Space)))))
    (format t "Puzzle1617 Solver Program ~%")
    (help-formula-to-input)
    (call-solver-grad-beam
     ;; todo: not by number, by problem file name.
     (nth (parse-integer (nth 0 arg-list))
          *problem-list-official*))))


