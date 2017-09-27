(in-package #:procon-2016)


;; commands


(defun eval-command (synthed-pieces token-list host-candi candidate retry-time)
  (let* ((tokens (car token-list))
         (command (car tokens))
         (argv1 (cadr tokens)))
    (format t "input data : ~A~%"  tokens)
    (cond ((eq command 're)
           (command-retry synthed-pieces retry-time))
          ((and (eq command 'sy) (numberp argv1))
           (command-synth argv1 synthed-pieces host-candi candidate))
          (t
           (command-error synthed-pieces host-candi candidate retry-time))
          )))


(defun command-error (synthed-pieces host-candi candidate retry-time)
  (format t "command error ~%")
  (read-command-util synthed-pieces host-candi candidate retry-time))

(defun command-retry (synthed-pieces retry-time)
  (format t "passed to synth. next : ~A th candi~%" (1+ retry-time))
  (cons synthed-pieces (1+ retry-time)))

(defun command-synth (number synthed-pieces host-candi candidate)
  (let* (;; new-candidate
         (local-candi (cadr (rotate-nth number candidate)))
         (new-candidate
          (piece-to-synth-piece
           (let-synthesize-piece-condition local-candi host-candi)))
         ;; remove-used-candi from synthed-pieces
         (removed-local-candi
          (remove-if #'(lambda (sy-piece)
                         (equalp (synth-piece-piece sy-piece)
                                 (piece-condition-piece local-candi)))
                     synthed-pieces))
         (remove-host-candi
          (remove-if #'(lambda (sy-piece)
                         (equalp (synth-piece-piece sy-piece)
                                 (piece-condition-piece host-candi)))
                     removed-local-candi))
         ;; next-synth
         (next-synthed-pieces (cons new-candidate remove-host-candi)))
    (format t "synthed piece, length whould be ~a~%" (length next-synthed-pieces))
    (cons next-synthed-pieces 0)))

