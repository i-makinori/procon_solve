(in-package :puzzle-1617)


;;;; Search in Gradient stacked (memoed) Beam search method
;;
;; # about searching method
;; this searching method is derivertive of Beam Search method.
;; when beam reach to timeout (now, tis by n_step) or local solution(it is not implemented now),
;; restart beam which is chosen from gradient stored in stack.


;;; parameters and variables

(defparameter *grad-stack-width-const* 1000 "max stack width for search of its score memo(grad)")


;;; structure


;;; functions for Gradient stacked Beam Search


;; filter for fs

(defun composition-of-filters
    (fs filters
     &key (cut-function #'cut-function-for-evaluation-value-by-delta-points_sum))
  ;; filter written in function composition form
  cut-function
  (not ;; do NOT satisfy all conditions == (not (or ...))
   (or
    (funcall cut-function fs)
    (detect-no-future-state fs)
    (find-if
     #'(lambda (filter_n)
         (let ((piece_fs (fs-frame-piece fs))
               (piece_fn (fs-frame-piece filter_n)))
           (or (detect-piece-congruent piece_fs piece_fn)
               (detect-domain-of-plus-piece-overs-frame piece_fs piece_fn))))
     filters))))


;; evaluation value updator for fs
#|
(defun fs-decrease-d/dt-evaluation-by-retake (fs)
  ;; greedic version
  "retake its state, gradient (d/dt) of evaluation value is decreased"
  ;; todo: convergence sequence
  ;; lim (n->infinity) => (f(n)->C) , C such as +2
  (setf (fs-evaluation-value fs)
        ((lambda (v)
           ;;(* v (/ 1 (+ 1 0.10))))
           (* v (expt (exp 1) -1/3)))
         (fs-evaluation-value fs)))
  fs)
|#


(defun fs-decrease-d/dt-evaluation-by-retake (fs)
;; d/dt version
  "retake its state, gradient (d/dt) of evaluation value is decreased"
  ;; todo: convergence sequence
  ;; lim (n->infinity) => (f(n)->C) , C such as +2
  (setf (fs-d/dt-evaluation-value fs)
        ((lambda (v)
           ;;(* v (/ 1 (+ 1 0.05)))) ;; V_next = V_now * 1 / (1+ε)
           (* v (expt (exp 1) -1/3)))
  (fs-d/dt-evaluation-value fs)))
  fs)


;; insert to stack
#|
(defun insert-state-into-stack-by-grad (fs stack-by-grad) ;; (state)
  ;; greedic version
  (cond ((composition-of-filters fs stack-by-grad)
         (insert fs stack-by-grad
                 :older-function #'(lambda (fs stack_n)
                                     #|(> (fs-d/dt-evaluation-value fs)
                                     (fs-d/dt-evaluation-value stack_n)))))
                                      |#
                                     (> (fs-evaluation-value fs)
                                        (fs-evaluation-value stack_n)))))
        (t stack-by-grad)))

|#


(defun insert-state-into-stack-by-grad (fs stack-by-grad) ;; (state)
  ;; d/dt version
  (cond ((composition-of-filters fs stack-by-grad)
         (insert fs stack-by-grad
                 :older-function #'(lambda (fs stack_n)
                                     (> (fs-d/dt-evaluation-value fs)
                                     (fs-d/dt-evaluation-value stack_n)))))
        (t stack-by-grad)))


(defun insert-state-list-into-stack-by-grad (fs-list stack-by-grad)
  (reduce #'(lambda (fs stack-by-grad)
              (insert-state-into-stack-by-grad  fs stack-by-grad))
          fs-list :initial-value stack-by-grad  :from-end t))



;;; partial procedures for search

;; beam, statement, gradient-stack, and etc.

(defun new-beam-and-next-gradient-stack-from-previous-gradient-stack (previous-gradient-stack)
  (incf *beam-current-index*)
  (let* ((new-beam
           (make-beam :index *beam-current-index*
                      :depth 0
                      ;; todo take state from once previous synthesize
                      :stack (list (nth 0 previous-gradient-stack))))
         (next-gradient-stack
           (insert-state-into-stack-by-grad ;; reinsert updated grad take/into gradient-stack
            (fs-decrease-d/dt-evaluation-by-retake (car previous-gradient-stack))
            (identity (cdr previous-gradient-stack)))))
    (values new-beam next-gradient-stack)))


(defun states-of-next-step-from-1-state-additional-filter
    (state-this-step primary-piece-list additional-filter)
  ;; todo: treatment congruent piece state between each beam-stack and gradient-stack
  (let* ((states-of-next-step_fat
           (states-of-next-step-from-1-state state-this-step primary-piece-list))
         ;; congruent filter, its beam and past gradient-stack
         (states-of-next-step
           (remove-if #'(lambda (state-this-step_i)
                          (some #'(lambda (filter-state_j)
                                    (detect-piece-congruent (fs-frame-piece state-this-step_i)
                                                            (fs-frame-piece filter-state_j)))
                                additional-filter))
                      states-of-next-step_fat)))

    states-of-next-step))

(defun next-gradient-stack (next-stack-of-this-step previous-gradient-stack)
  (first-n *grad-stack-width-const*
           (insert-state-list-into-stack-by-grad ;; insert list of states
            next-stack-of-this-step previous-gradient-stack)))

(defun forward-to-next-beam-and-next-gradient-stack
    (previous-beam-of-step primary-piece-list previous-gradient-stack)
  (let ((previous-state-of-step (car (beam-stack previous-beam-of-step)))
        (previous-stacking-of-step (cdr (beam-stack previous-beam-of-step))))
    (let* ((states-of-next-step
             ;; todo: treatment congruent piece state between each beam-stack and gradient-stack
             (states-of-next-step-from-1-state-additional-filter
              previous-state-of-step primary-piece-list previous-gradient-stack))
           (next-stack-of-this-step
             (next-state-stack states-of-next-step previous-stacking-of-step))
           (next-beam-of-this-step
             (beam-next previous-beam-of-step next-stack-of-this-step))
           (next-gradient-stack
             (next-gradient-stack next-stack-of-this-step previous-gradient-stack)))
    (values next-beam-of-this-step next-gradient-stack next-stack-of-this-step))))


;; format. I/O

(defun format-search-status-before-for-grad-beam (beam-of-this-step primary-piece-list)
  (let ((state-of-this-step (car (beam-stack beam-of-this-step))))
    (format-search-status-before state-of-this-step primary-piece-list)
    (format t "beam {type, depth}: {~A, ~A}~%"
            (beam-index beam-of-this-step) (beam-depth beam-of-this-step))
    (format t "d/dt: ~A~%" (fs-d/dt-evaluation-value state-of-this-step))))

(defun format-search-status-after-for-grad-beam (next-beam-of-this-step next-gradient-stack)
  (let ((next-stack-of-this-step (beam-stack next-beam-of-this-step)))
    (format-search-status-after next-stack-of-this-step)
    (format t "grad length: ~A~%"
            (length next-gradient-stack))
    (format t "  (first_20-d/dt) ... : ~A ... ~%"
            (mapcar #'(lambda (s) (format nil "~,4f " (fs-d/dt-evaluation-value s)))
                    (first-n 20 next-gradient-stack)))))


(let ((++timestamp-next-html-write-after-for-grad-beam++ (local-time:now)))
  (defun write-piece-list-as-html-from-fs-stacks-for-grad-beam
      (next-stack-of-this-step gradient-stack &optional (by-delta-time-p-millisec nil))
    (when (or (not (numberp by-delta-time-p-millisec))
              (local-time:timestamp>= (local-time:now)
                                      ++timestamp-next-html-write-after-for-grad-beam++))
      (write-piece-list-as-html
       (mapcar #'(lambda (state) (fs-frame-piece state)) next-stack-of-this-step)
       :file-name "piece-list.html")
      (write-piece-list-as-html
       (mapcar #'(lambda (state) (fs-frame-piece state)) gradient-stack)
       :file-name "gradient-list.html")
      (when (numberp by-delta-time-p-millisec)
        (setf ++timestamp-next-html-write-after-for-grad-beam++
              (local-time:timestamp+ (local-time:now)
                                     (* by-delta-time-p-millisec (expt 1000 2))
                                     :nsec))))))



;;; Gradient stacked Beam Search

;; minus to frame method

(defun search-solution-aux-grad-beam (beam-queue primary-piece-list gradient-stack)
  (let* ((beam-of-this-step (car beam-queue))
         (rest-queue (cdr beam-queue)))
    (cond
      ((and (null gradient-stack) (null beam-queue)) ;; unexists any states
       (format t "====== no states, and end ======~%")
       nil)
      (;; not enough beams
       ;;(and (< (length beam-queue) *beam-width*) (not (null gradient-stack)))
       ;; not enough beams and need to search in wide
       (and (< (length beam-queue) *beam-width*) (not (null gradient-stack))
            (not (= 1 (length beam-queue))))
       (format t "=== new beam ===~%")
       (multiple-value-bind (new-beam next-gradient-stack)
           (new-beam-and-next-gradient-stack-from-previous-gradient-stack gradient-stack)
         ;; search with new beam
         (search-solution-aux-grad-beam (append beam-queue (list new-beam))
                                        primary-piece-list next-gradient-stack)))
      (;; too deep
       (> (beam-depth beam-of-this-step) *n-search-iter-max*)
       (format t "=== restart beam ===~%")
       (search-solution-aux-grad-beam rest-queue primary-piece-list gradient-stack))
      (t ;; otherwise
       ;; "============" ;; format before forward once list
       (format-search-status-before-for-grad-beam beam-of-this-step primary-piece-list)
       ;;
       (multiple-value-bind (next-beam-of-this-step
                             next-gradient-stack
                             next-stack-of-this-step)
           (forward-to-next-beam-and-next-gradient-stack beam-of-this-step
                                                         primary-piece-list gradient-stack)
         ;;              ;; format after once list
         (format-search-status-after-for-grad-beam next-beam-of-this-step next-gradient-stack)
         ;;              ;; HTML by 10000[ms]
         (write-piece-list-as-html-from-fs-stacks-for-grad-beam
          next-stack-of-this-step next-gradient-stack
          10000)
         ;; Call nexts or gool
         (cond
           ((null next-stack-of-this-step) ;; no methods in this beam's stack
            (format t "=== the Beam_n is Cut By Dead Twig ===~%")
            (search-solution-aux-grad-beam
             rest-queue
             primary-piece-list next-gradient-stack))
           ((state-is-solution-p (car next-stack-of-this-step))
            ;; car is solution, return all states of its beam.
            (values next-stack-of-this-step gradient-stack))
           (t ;; search with next beam
            (search-solution-aux-grad-beam (append rest-queue (list next-beam-of-this-step))
                                           primary-piece-list next-gradient-stack)))
         )))))

(defun search-solution-grad-beam (whole-primary-piece-list)
  (let* (;; search easy before search frame.
         ;;(unique-piece-list (search-unique-synthesize-bfs whole-primary-piece-list))
         (unique-piece-list (identity whole-primary-piece-list))         
         ;; params
         ;;(primary-pieces (remove-if-not #'primary-piece-p unique-piece-list))
         (primary-pieces unique-piece-list)
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
    ;; variables
    (init-meta-params primary-pieces
                      :iter-max (ceiling (* (length unique-piece-list) 6/4))
                      :stack-width-const-1 (ceiling (* (length unique-piece-list) 5/2))
                      :beam-width 40)

    ;; handle problem forms
    (cond
      ((not (= 1 (length frame-pieces)))
       (warn (format nil "whole-piece-list has multiple frames. IDs: ~A~%"
                     (mapcar #'piece-id frame-pieces)))
       nil)
      (t
       ;; call search-aux
       (let* ((frame-piece    (car frame-pieces))
              ;;
              (none-frame-pieces
                (list-of-unused-primary-piece-list-of-synthesized-piece frame-piece primary-pieces))
              ;; t0
              (stack_t0 (list (make-fs-from-piece frame-piece none-frame-pieces)))
              ;; t1
              (stack_t1 (next-state-stack
                         (states-of-next-step-from-1-state (car stack_t0) none-frame-pieces)
                         '()))
              (beam-queue_t1
                (generate-initial-beams *beam-width* stack_t1))
              (gradient-stack_t0
                (insert-state-list-into-stack-by-grad stack_t1 '())))
         (multiple-value-bind (solution-and-paths gradient-stack)
             (search-solution-aux-grad-beam
              beam-queue_t1 none-frame-pieces gradient-stack_t0)
           (write-piece-list-as-html-from-fs-stacks-for-grad-beam
            solution-and-paths gradient-stack)
           (mapcar #'(lambda (s) (fs-frame-piece s)) solution-and-paths)))
         ))))
