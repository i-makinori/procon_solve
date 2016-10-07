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

(defun read-command (&optional (file-name *command-file*))
  
  )

(defun eval-command (token)
  )

(defun evalute-piece-part (piece)
  )

(defun write-candidate (condi-list &optional piece)
  )

(defun mainpulate-candidate (synth-piece-list)
  (synth-piece-to-candidate synth-piece-list))

(defun repl (synthed-pieces)
  " read eval print loop"
  synthed-pieces
  (let ((piece-point-evalut ()))
    
  ))

