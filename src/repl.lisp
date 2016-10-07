(in-package #:procon)

;;;; defines

(defparameter *puzzle-hash* (make-hash-table :test #'equalp))
;;; (*puzzle-hash* <- \x) :: piece | synthed-piece |  |

(defstruct synth-piece "synthesized piece"
           (piece '())
           (piece1-condition '())
           (piece1-synth-piece '())
           (piece2-condition '())
           (piece2-synth-piece '())
           (synthed-piece '())
           (no '())
           )

;;;; repl

(defun repl (synthed-pieces)
  " read eval print loop"
  synthed-pieces
  (let* ((candidate-list-cons (synth-piece-to-candidate synthed-pieces))
         (candidate-list (cdr candidate-list-cons))
         (host-candi (car candidate-list-cons)))
    (write-candidate host-candi candidate-list)
    
  ))

;;;read-command
(defun read-command (&optional (file-name *command-file*))
  )




;;; write-candi-date

#|
(defun write-candidate (host-candi candidate)
  (write-candidate-file
   (reduce #'append
           (mapcar #'(lambda (candi)
                       (list (list (car candi) (cadr host-candi) (caddr host-candi))
                             candi))
                   candidate))))
|#

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
  ))








