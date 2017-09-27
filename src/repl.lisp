(in-package #:procon-2016)

;;;; defines

(defparameter *puzzle-hash* (make-hash-table :test #'equalp))
;;; (*puzzle-hash* <- \x) :: piece | synthed-piece |  |

(defstruct synth-piece "synthesized piece"
           (piece '())
           (piece1-condition '())
           (piece1-synth-piece '())
           (piece2-condition '())
           (piece2-synth-piece '())
           ;;(synthed-piece '())
           (no '())
           )

;;;; repl

(defun repl (synthed-pieces retry-time)
  " read eval print loop"
  synthed-pieces
  (let* (;; search and change-type
         (candidate-list-cons (synth-piece-to-candidate synthed-pieces retry-time))
         (candidate-list (cdr candidate-list-cons))
         (host-candi (car candidate-list-cons))
         (candi-number-pair candidate-list)
         ;; svg html
         (do1 (write-svg host-candi candi-number-pair))
         ;; read command
         (do2 (progn (format t "================================================~%")))
         (command-result 
          (read-command-util synthed-pieces 
                             host-candi candidate-list retry-time)))
    (repl (car command-result) (cdr command-result))
    ))


(defun call-repl ()
  (repl (mapcar #'piece-to-synth-piece (read-coord-data)) 0))

;;;read-command

(defun read-command-util (synthed-pieces host-candi candidate retry-time)
  (let* ((text ""))
    (cond ((null candidate) (command-retry synthed-pieces retry-time))
          (t (format t "input command ~% user >> ")
             (setq text (read-line))
             (eval-command synthed-pieces (parse-to-token text)
                           host-candi candidate retry-time))
  )))







;;; write-candi-svg


;;; write-candi-date


#|
(defun write-candidate (host-candi candidate)
  (write-candidate-file
   (reduce #'append
           (mapcar #'(lambda (candi)
                       (let ((num (car candi))
                             (candi (cdr candi)))
                         (list (list num (cadr host-candi) (caddr host-candi))
                               (list num (cadr candi) (caddr candi )))))
                   (number-pair candidate)))))


(defun coord-text (coord-list)
  (reduce #'(lambda (str1 str2)
              (concatenate 'string str1 str2))
          (mapcar #'(lambda (c)
                      (format nil "~a ~a " (round (car c)) (round (cdr c))))
                  coord-list)
          ))


(defun candidate-line-text (priority piece-no piece-condition)
  (let* ((coord (piece-vectors (piece-condition-piece piece-condition)))
         (coord-text (coord-text coord))
         (two-side (piece-condition-two-side piece-condition))
         (two-side-text (if two-side "T" "F"))
         (deg (piece-condition-angle piece-condition)))
    (format nil "~a ~a ~a/ ~a ~5$~%" priority piece-no coord-text two-side-text deg)))

(defun write-candidate-file (candidate-list &optional (file-name *candidate-file*))
  "not perfect implementation"
  (let* ((text (reduce #'(lambda (s1 s2)
                           (concatenate 'string s1 s2))
                       (mapcar #'(lambda (x)
                                   (candidate-line-text (car x) (cadr x) (caddr x)))
                               candidate-list))))
    (write-file file-name text)
    candidate-list
  ))




|#
