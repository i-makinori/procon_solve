
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


#|
(defun unique-detection-for-2nd-step (synthesized-piece-list primary-piece-list)
  (let ((list-of-nil-and-sy_x
          (remove nil
                  (mapcar
                   #'(lambda (sy_n)
                       (multiple-value-bind (synthed-pieces state)
                           (rare-synthesizeables-of-pieces-to-piece-by-partial-problem-evaluations
                            sy_n
                            (list-of-unused-primary-piece-list-of-synthesized-piece
                             ;;synthesized-piece-list
                             sy_n
                             primary-piece-list)
                            primary-piece-list)
                         synthed-pieces state
                         (if (not (null state)) sy_n nil)))
                   synthesized-piece-list))))
    (if (= 1 (length list-of-nil-and-sy_x))
        (nth 0 list-of-nil-and-sy_x)
        nil)))
|#


(defun maybe-unique-synthesize-to-piece-1step (piece piece-list primary-piece-list)
  (multiple-value-bind (synthed-pieces state)
      (rare-synthesizeables-of-pieces-to-piece-by-partial-problem-evaluations
       piece piece-list primary-piece-list)
    (cond ((eq state 'unique-synthesizes)
           (let ((synthed-piece (car synthed-pieces)))
             (setf (piece-leaf-or-synthed synthed-piece) 'leaf)
             synthed-piece))
          ((eq state 'converge-synthesizes)
           #|
           (let ((maybe-unique
                   (unique-detection-for-2nd-step synthed-pieces primary-piece-list)))
             (if maybe-unique
                 (progn (setf (piece-leaf-or-synthed maybe-unique) 'leaf)
                        maybe-unique)
                 piece)))
           |#
           piece)
          (t piece))))

;; write html

;;(defparameter *timestamp-next-html-write-after-for-unique-search* (local-time:now))

(let ((++timestamp-next-html-write-after-for-unique-search++ (local-time:now)))
  (defun write-piece-list-as-html-from-fs-stacks-for-unique-search
      (unique-piece-list &optional (by-delta-time-p-millisec nil))
    (when (or (not (numberp by-delta-time-p-millisec))
              (local-time:timestamp>= (local-time:now)
                                      ++timestamp-next-html-write-after-for-unique-search++))
      (write-piece-list-as-html
       ;;(mapcar #'(lambda (state) (fs-frame-piece state)) gradient-stack)
       unique-piece-list
       :file-name "unique-piece-list.html")
      
      (when (numberp by-delta-time-p-millisec)
        (setf ++timestamp-next-html-write-after-for-unique-search++
              (local-time:timestamp+ (local-time:now)
                                     (* by-delta-time-p-millisec (expt 1000 2))
                                     :nsec))))))


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
     (let* ((primary-piece-list-whole (append primary-piece-list-currents primary-piece-list-next))
            (piece-synth-to (car primary-piece-list-currents))
            (piece-synth-for (cdr primary-piece-list-whole))
            ;;
            (maybe-unique-synthesize
              (maybe-unique-synthesize-to-piece-1step
               piece-synth-to piece-synth-for primary-piece-list-whole)))
       ;; format
       ;;              ;; HTML by 10000[ms]
       (write-piece-list-as-html-from-fs-stacks-for-unique-search
        (append primary-piece-list-currents primary-piece-list-next)
        10000)
       ;; search next (demi) prime-piece
       (if (equal (piece-id piece-synth-to) (piece-id maybe-unique-synthesize))
           (search-unique-synthesize-bfs-aux
            first-primary-piece-list
            (list-of-unused-piece-list-of-piece maybe-unique-synthesize
                                                primary-piece-list-currents)
            (cons maybe-unique-synthesize
                  (list-of-unused-piece-list-of-piece maybe-unique-synthesize
                                                      primary-piece-list-next)))
           (search-unique-synthesize-bfs-aux
            first-primary-piece-list
            (cons maybe-unique-synthesize
                  (list-of-unused-piece-list-of-piece maybe-unique-synthesize
                                                      primary-piece-list-currents))
            (list-of-unused-piece-list-of-piece maybe-unique-synthesize
                                                primary-piece-list-next))
           )))))



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
         ;; render to html
         (write-piece-list-as-html-from-fs-stacks-for-unique-search unique-synthesizes)
         ;; return
         unique-synthesizes)))))
