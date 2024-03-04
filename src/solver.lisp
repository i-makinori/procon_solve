
(in-package :puzzle-1617)

;;;; solver


;;;; search solution

;; minus to frame method

#|
;; usage
(search-solution-from-prime-pieces
 (cons (car *example-problem-9*) (cdr *example-problem-9*)))
|#

(defstruct (fs-node (:constructor make-fs) (:conc-name fs-))
  ;; frame-state of node
  (:frame-piece nil)
  (:evaluation-value nil)
  (:primary-used nil) ;; primary pieces used
  (:primary-rest nil) ;; primary pieces resting
  )

(defun format-search-status-before (state-of-this-step primary-piece-list)
  primary-piece-list
    (format t "~%============~%")
    (format t "synth-list to: ~A,~%"
            (piece-id (fs-frame-piece state-of-this-step)))
    (format t "using-pieces [len]: ~A[~A]~%"
            (mapcar #'piece-id (fs-primary-used state-of-this-step))
            (length (fs-primary-used state-of-this-step))))

#|
(defun filter-piece-list-from-synthesized-piece-list
    (state primary-piece-list synthesized-piece-list)
  ;; call filter functions, and sort.
  (remove-plus-piece-overs-frame-from-synthesized-piece-list
   (remove-no-future-shaped-piece-from-synthesized-piece-list
    (remove-congruent-from-synthesized-piece-list synthesized-piece-list)
    primary-piece-list)
   ;; todo. this is once old frame. frame of this step is may be better
   (fs-frame-piece state)))
|#

(defun filter-fs-list (fs-list state-this-step) ;; (state-list)
  ;; call filter functions, and sort.
  (remove-plus-piece-overs-frame-from-state-list
   (remove-no-future-state-from-state-list
    (remove-congruent-from-state-list fs-list))
   ;; todo. this is once old frame. frame of this step is may be better
   state-this-step))


(defun states-of-next-step-from-1-state (state-this-step primary-piece-list)
  (let* (;; Synthesized Piece List
         (spl-all-combinations
           (funcall *step-function*
                    (fs-frame-piece state-this-step)
                    (fs-primary-rest state-this-step)))
         (fs-all-combinations
           (mapcar #'(lambda (next-piece)
                       (make-fs-from-piece next-piece primary-piece-list))
                   spl-all-combinations))
         (spl-filtered ;; patterns-of-step
           (filter-fs-list fs-all-combinations state-this-step))
         ;; stack of states
         (states-of-next-step spl-filtered)
         )
    states-of-next-step))

(defun format-search-status-after (next-stack)
  next-stack
  (format t "EvalValues:")
  (mapcar #'(lambda (s) (format t " ~,4f" (fs-evaluation-value s)))
          (first-n 20 next-stack))
  (format t "~%")
  nil)


(defun next-state-stack (states-new states-rest)
  (sorted-states-by-evaluation-function
   states-new states-rest))

;;; greede DFS

(defparameter *step-function*
  ;;#'all-synthesizeable-patterns-of-pieces-to-frame
  #'all-synthesizeables-of-pieces-to-piece_del-if-e-jam-edge
  "step function to get next pieces"
  )

(defparameter *evaluation-function*
  #'evaluation-value-by-delta-points_sum
  ;;#'evaluation-value-by-delta-points_delta
  "evaluation funciton of step (node or edge is not determined)"
  )


(defun make-fs-from-piece (synthed-frame-piece primary-piece-list)
  (let ((fs1
          (make-fs :frame-piece synthed-frame-piece
                   :primary-used (list-of-primary-piece-list-of-synthesized-piece
                                  synthed-frame-piece)
                   :primary-rest (list-of-unused-primary-piece-list-of-synthesized-piece
                                  synthed-frame-piece primary-piece-list))))
    (progn
      (setf (fs-evaluation-value fs1)
            (funcall *evaluation-function* fs1 primary-piece-list))
      fs1)))

(defparameter *n-search-iter* 0)
(defparameter *n-search-iter-max* 4000)

(defun search-solution-aux (stack-of-states primary-piece-list)
  (let* ((state (car stack-of-states))
         (stacking (cdr stack-of-states)))
    ;; format before once list 
    (format-search-status-before state primary-piece-list)
    (let* ((states-of-next-step (states-of-next-step-from-1-state state primary-piece-list))
           (next-stack (next-state-stack states-of-next-step stacking)))
      ;; format after once list
      (format-search-status-after next-stack)
      ;; HTML
      (write-piece-list-as-html 
       (mapcar #'(lambda (state) (fs-frame-piece state)) next-stack))
      ;; finish or recursive
      (incf *n-search-iter*)
      (cond ((null next-stack) ;; no methods
             (format t "there is no solutions. IDs: ~A~%" (mapcar #'piece-id primary-piece-list))
             nil)
            ((zero-shape-piece-p (fs-frame-piece (car next-stack)))
             ;; car is solution, however return all
             next-stack)
            ((> *n-search-iter* *n-search-iter-max*)
             (format t "could not get solutions in ~A trial.~%" *n-search-iter*)
             nil)
            (t ;; search-next
             (search-solution-aux next-stack primary-piece-list)))
      )))

(defun search-solution-from-prime-pieces (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
    (setf *n-search-iter* 0)
    (cond
      ((not (= 1 (length frame-pieces)))
       (warn (format nil "whole-piece-list has multiple frames. IDs: ~A~%"
                     (mapcar #'piece-id frame-pieces)))
       nil)
      (t 
       (let* ((frame-piece    (car frame-pieces))
              ;;
              (none-frame-pieces (remove frame-piece primary-pieces :test #'equalp))
              (stack-of-states_t0
                (list (make-fs-from-piece frame-piece none-frame-pieces)))
              (solution-and-paths (search-solution-aux stack-of-states_t0 none-frame-pieces)))
         (mapcar #'(lambda (s) (fs-frame-piece s)) solution-and-paths)
)))))



;;;; BFS (delta_edge > 2)

#|
(defparameter *state1*
  `(;; list of state
    (:evaluation-of-edge 0)
    (:piece (piece))))
|#
