
(in-package :puzzle-1617)

;;;; Search in unique-synthesized piece in (Breadth-First Search) method.

;; util
#|
(defun remove-congruent-shape-from-synthesized-piece-list (synthesized-piece-list)
  ;; remove congruent shaped synthesize
  ;; if Shape(P1 + P2) = Shape(P3 + P5) , P1 + P2 remains.
  (let ((lis synthesized-piece-list))
    (cond ((null lis) '())
          (t (cons (car lis)
                   (remove-congruent-shape-from-synthesized-piece-list
                    (remove-if #'(lambda (p)
                                   (detect-piece-congruent (car lis) p t))
                               (cdr lis))))))))
|#

;; synthesize piece

(defun maybe-unique-synthesize-to-piece-1step (piece piece-list primary-piece-list)
  (multiple-value-bind (synthed-pieces state)
      (rare-synthesizeables-of-pieces-to-piece-by-partial-problem-evaluations
       piece piece-list primary-piece-list)
    (cond ((eq state 'unique-synthesizes)
           (let ((synthed-piece (car synthed-pieces)))
             (setf (piece-leaf-or-synthed synthed-piece) 'leaf)
             synthed-piece))
          (t piece))))

;; search

(defun search-unique-synthesize-bfs-aux
    (first-primary-piece-list primary-piece-list-currents primary-piece-list-next)
  (cond
    ((null primary-piece-list-currents)
     ;; reset dictionaries
     (setf *partial-angle-dictionary* (make-dictionary))
     (setf *partial-length^2-dictionary* (make-dictionary))
     ;; end to search or search next step
     (cond ((set-equal (mapcar #'piece-id first-primary-piece-list)
                       (mapcar #'piece-id primary-piece-list-next))
            ;; end to search
            first-primary-piece-list)
           (t
            ;; search next stage
            (search-unique-synthesize-bfs-aux
             primary-piece-list-next primary-piece-list-next '()))))
    (t
     (let* ((piece-synth-to (car primary-piece-list-currents))
            (primary-piece-list-whole (append primary-piece-list-currents primary-piece-list-next))
            (piece-synth-for (cdr primary-piece-list-whole))
            ;;
            (maybe-unique-synthesize
              (maybe-unique-synthesize-to-piece-1step
               piece-synth-to piece-synth-for primary-piece-list-whole)))
       ;; format
       ;;(write-piece-list-as-html (append primary-piece-list-currents primary-piece-list-next))
       ;; search next (demi) prime-piece
       (search-unique-synthesize-bfs-aux
        first-primary-piece-list
        (list-of-unused-piece-list-of-piece maybe-unique-synthesize primary-piece-list-currents)
        (cons
         maybe-unique-synthesize
         (list-of-unused-piece-list-of-piece maybe-unique-synthesize primary-piece-list-next)
         ))))))



(defun search-unique-synthesize-bfs (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
    ;;
    (init-meta-params whole-primary-piece-list)
    ;;
    (cond
      ;; handle error
      ((not (= 1 (length frame-pieces)))
       (warn (format nil "whole-piece-list has multiple frames. IDs: ~A~%"
                     (mapcar #'piece-id frame-pieces)))
       nil)
      (t
       ;; init
       (init-meta-params primary-pieces
                         :iter-max 4000)
       ;; call-search
       (let* ((unique-synthesizes
                (search-unique-synthesize-bfs-aux
                 whole-primary-piece-list whole-primary-piece-list '())))
         unique-synthesizes)))))
