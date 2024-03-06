(in-package :puzzle-1617)


;;;; Search in Gradient stacked (memoed) Beam search method
;;
;; # about searching method
;; this searching method is derivertive of Beam Search method.
;; when beam reach to timeout (now, tis by n_step) or local solution(it is todo now),
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

(defun fs-decrease-d/dt-evaluation-by-retake (fs)
  "retake its state, gradient (d/dt) of evaluation value is decreased"
  ;; todo: convergence sequence 
  ;; lim (n->infinity) => (f(n)->C) , C such as +2
  (setf (fs-d/dt-evaluation-value fs)
        ((lambda (v)
           (* v (/ 1 (+ 1 0.05)))) ;; V_next = V_now * 1 / (1+ε)
         (fs-d/dt-evaluation-value fs)))
  fs)


;; insert to stack

(defun insert-state-into-stack-by-grad (fs stack-by-grad) ;; (state)
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


;; partial procedures for search

(defun states-of-next-step-from-1-state-additional-filter
 (state-this-step primary-piece-list additional-filter)
  (let* ((states-of-next-step_fat
           (states-of-next-step-from-1-state state-this-step primary-piece-list))
         (states-of-next-step
           (remove-if #'(lambda (state-this-step_i)
                          (some #'(lambda (filter-state_j)
                                    (detect-piece-congruent (fs-frame-piece state-this-step_i)
                                                            (fs-frame-piece filter-state_j)))
                                additional-filter))
                      states-of-next-step_fat)))
    states-of-next-step))


;;; Gradient stacked Beam Search

;; minus to frame method

(defun search-solution-aux-grad-beam (beam-queue primary-piece-list gradient-stack)
  (let* (;; 
         (beam-of-this-step (car beam-queue))
         (rest-queue (cdr beam-queue))
         ;;
         (state-of-this-step (car (beam-stack beam-of-this-step)))
         (stacking-of-this-step (cdr (beam-stack beam-of-this-step))))
    (cond
      (;; not enough beams
       (and (< (length beam-queue) *beam-width*)
            (car gradient-stack)) ;; exists
       (format t "====== new beam ======~%")
       (incf *beam-current-index*)
       (let* ((new-beam
                (make-beam :index *beam-current-index*
                           :depth 0
                           :stack (list (nth 0 gradient-stack))))
              (next-gradient-stack
                (insert-state-into-stack-by-grad ;; insert one state
                 (fs-decrease-d/dt-evaluation-by-retake (car gradient-stack))
                 (identity (cdr gradient-stack)))))
         (search-solution-aux-grad-beam (append beam-queue (list new-beam))
                                        primary-piece-list next-gradient-stack)))
      (;; too deep
       (> (beam-depth beam-of-this-step) *n-search-iter-max*)
       (format t "======= restart beam =======~%")
       (search-solution-aux-grad-beam rest-queue primary-piece-list gradient-stack))
      (t ;; otherwise
       ;; format before once list 
       (format-search-status-before state-of-this-step primary-piece-list)
       (format t "beam {type, depth}: {~A, ~A}~%"
               (beam-index beam-of-this-step) (beam-depth beam-of-this-step))
       (format t "d/dt: ~A~%" (fs-d/dt-evaluation-value state-of-this-step))
       (let* ((states-of-next-step
                (states-of-next-step-from-1-state-additional-filter
                 state-of-this-step primary-piece-list gradient-stack))
              (next-stack-of-this-step
                (next-state-stack states-of-next-step stacking-of-this-step))
              ;;
              (next-gradient-stack
                (first-n *grad-stack-width-const*
                         (insert-state-list-into-stack-by-grad ;; insert list of states
                          next-stack-of-this-step gradient-stack)))
              ;;
              (next-beam-of-this-step (beam-next beam-of-this-step next-stack-of-this-step)))
         ;; format after once list
         (format-search-status-after next-stack-of-this-step)
         (format t "grad length: ~A~%"
                 (length next-gradient-stack))
         (format t "  (first_20-d/dt) ... : ~A ... ~%"
                 (mapcar #'(lambda (s) (format nil "~,4f " (fs-d/dt-evaluation-value s)))
                         (first-n 20 next-gradient-stack)))
         ;; HTML
         (write-piece-list-as-html 
          (mapcar #'(lambda (state) (fs-frame-piece state)) next-stack-of-this-step)
          :file-name "piece-list.html")
         (write-piece-list-as-html 
          (mapcar #'(lambda (state) (fs-frame-piece state)) gradient-stack)
          :file-name "gradient-list.html")

         ;; Call nexts or gool
         (cond
           ((null next-stack-of-this-step) ;; no methods in this beam's stack
            (format t "=== the Beam_n is Cut By Dead Twig ===~%")
            (search-solution-aux-grad-beam 
             rest-queue
             primary-piece-list next-gradient-stack))
           ((state-is-solution-p (car next-stack-of-this-step))
            ;; car is solution, return all states its beam.
            next-stack-of-this-step)
           (t ;; search-next
            (search-solution-aux-grad-beam
             (append rest-queue (list next-beam-of-this-step))
             primary-piece-list next-gradient-stack)))
         )))))

(defun search-solution-grad-beam (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
    ;; variables
    (setf *n-search-iter* 0)
    (setf *beam-current-index* 0)
    ;; parameters
    (setf *beam-width* 6)
    (setf *n-search-iter-max* 10) ;; n:: depth

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
              (stack0 (list (make-fs-from-piece frame-piece none-frame-pieces)))
              ;;
              (stack-of-states_t0
                (list (make-beam :index 0
                                 :stack stack0)))
              (gradient-stack_t0
                (insert-state-list-into-stack-by-grad stack0 '()))
              ;;
              (solution-and-paths (search-solution-aux-grad-beam
                                   stack-of-states_t0 none-frame-pieces gradient-stack_t0)))
         
         (mapcar #'(lambda (s) (fs-frame-piece s)) solution-and-paths)
         )))))
