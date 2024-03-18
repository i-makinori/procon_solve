
(in-package :puzzle-1617)

;;;; Search in DFS (Depth First Search) method.
;;
;; # about DFS
;; simple Depth First Search
;; in one step, step takes best evaluation valued choice from next avaiable choices.
;; other choices are stored in sorted stack.
;;

;;; parameters and variables

(defparameter *n-search-iter* 0) ;; variable
(defparameter *n-search-iter-max* 30) ;; parameter

(defparameter *beam-stack-width-const* 200 "max stack width for search of its beam") ;; parameter
(defparameter *fs-stackwidth-const* 100)

;;; function configs

(defparameter *step-function* #'identity)

(defparameter *evaluation-function* #'identity)

;;; structure

(defstruct (fs-node (:constructor make-fs) (:conc-name fs-))
  ;; Frame-Status-of-NODE,
  ;; called in fs, fs-node, status, ...
  (:frame-piece (piece))
  (:primary-used nil) ;; primary pieces used
  (:primary-rest nil) ;; primary pieces resting
  ;;
  (:evaluation-value nil)
  (:d/dt-evaluation-value nil))


;; make fs

(defun make-fs-from-piece (synthed-frame-piece primary-piece-list
                           &optional (state-ago (make-fs)))
  (let ((fs1
          (make-fs :frame-piece synthed-frame-piece
                   :primary-used (list-of-primary-piece-list-of-synthesized-piece
                                  synthed-frame-piece)
                   :primary-rest (list-of-unused-primary-piece-list-of-synthesized-piece
                                  synthed-frame-piece primary-piece-list))))
    (progn
      (setf (fs-evaluation-value fs1)
            (funcall *evaluation-function* fs1 primary-piece-list))
      (setf (fs-d/dt-evaluation-value fs1)
            (d/dt-evaluation-value
             (fs-evaluation-value fs1)
             (fs-evaluation-value state-ago)
             1))
      fs1)))


;; checking if it is solution

(defun state-is-solution-p (fs) ;; (state)
  (zero-shape-piece-p (fs-frame-piece fs)))


;;; functions for DFS Greede.

(defun init-meta-params (primary-piece-list
                         &key (iter-max 3000) (stack-width-const-1 200) (beam-width 6))
  ;;; parameters
  ;; 
  (setf *n-search-iter* 0) ;; variable
  (setf *n-search-iter-max* iter-max) ;; parameter
  ;; stack size
  (setf *beam-stack-width-const* 150) ;; todo "max stack width for search of its beam"
  (setf *fs-stackwidth-const* stack-width-const-1) ;; todo "max stack width for common storage"
  ;; beam param
  (setf *beam-current-index* 0)
  (setf *beam-width* beam-width)

  ;;; functions
  (setf *step-function*
        ;;#'all-synthesizeable-patterns-of-pieces-to-frame
        ;;#'all-synthesizeables-of-pieces-to-piece_del-if-e-jam-edge
        ;;#'rare-synthesizeables-of-pieces-to-piece
        ;;#'rare-synthesizeables-of-pieces-to-piece-_del-if-e-jam-edge
        #'rare-synthesizeables-of-pieces-to-piece-by-partial-problem-evaluations
        ;;#'synthesizeables-by-fusuon-method-for-stuffing ;; for debug
        )   ;;"step function to get next pieces"

  (setf *evaluation-function*
        ;;#'evaluation-value-by-delta-points_sum
        ;;#'evaluation-value-by-num-used-pieces
        ;;#'evaluation-value-by-remain-edges
        ;;#'evaluation-value-by-remain-edges-rest-better-synthesize
        ;;#'evaluation-value-by-remain-edge-combinations
        #'evaluation-value-by-remain-edges-in-reduce-in-k-step
        )


  ;; partial problem parameters
  (setf *partial-width-limit*
        (min 2000
             (* (num-combination-sequence 
                 1 (* 1/2 (length (sy-select-parameters-from-piece-list primary-piece-list))))
                0.33)))
  (setf *partial-iter-limit* ;; todo: are there some better iter limit?
        ;;(ceiling (/ *partial-width-limit* 2))) 
        (* 1/1 *partial-width-limit*))
  ;;; dictionaries
  (setf *partial-angle-dictionary* (make-dictionary))
  (setf *partial-length^2-dictionary* (make-dictionary))
  )

(defun format-search-status-before (state-of-this-step primary-piece-list)
  primary-piece-list
  (format t "~%============~%")
  (format t "synth-list to: ~A,~%"
          (piece-id (fs-frame-piece state-of-this-step)))
  (format t "using-pieces [len]: ~A[~A]~%"
          (mapcar #'piece-id (fs-primary-used state-of-this-step))
          (length (fs-primary-used state-of-this-step))))

(defun states-of-next-step-from-1-state (state-this-step primary-piece-list)
  (let* (;; Synthesized Piece List
         (spl-all-combinations
           (funcall *step-function*
                    (fs-frame-piece state-this-step)
                    (fs-primary-rest state-this-step)
                    primary-piece-list))
         (fs-all-combinations
           (mapcar #'(lambda (next-piece)
                       (make-fs-from-piece next-piece primary-piece-list state-this-step))
                   spl-all-combinations))
         (spl-filtered ;; patterns-of-step
           (filter-fs-list fs-all-combinations state-this-step))
         ;; stack of states
         (states-of-next-step spl-filtered))
    (format t "remains: ~A: ~A~%" (length fs-all-combinations) (length spl-filtered))
    states-of-next-step))

(defun next-state-stack (states-new states-rest)
  (sorted-states-by-evaluation-function
   states-new states-rest))

(defun format-search-status-after (next-stack)
  next-stack
  ;;(format t "EvalValues:")
  ;;(mapcar #'(lambda (s) (format t " ~,4f" (fs-evaluation-value s)))
  ;;(first-n 20 next-stack))
  ;;(format t "~%")
  nil)



;;; DFS Greede Search

;; minus to frame method

(defun search-solution-aux (stack-of-states primary-piece-list)
  (let* ((state-of-this-step (car stack-of-states))
         (stacking (cdr stack-of-states)))
    ;; format before once list
    (format-search-status-before state-of-this-step primary-piece-list)
    (let* ((states-of-next-step (states-of-next-step-from-1-state state-of-this-step
                                                                  primary-piece-list))
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
            ((state-is-solution-p (car next-stack))
             ;; car is solution, however return all
             next-stack)
            ((>= *n-search-iter* *n-search-iter-max*)
             (format t "could not get solutions in ~A trial.~%" *n-search-iter*)
             nil)
            (t ;; search-next
             (search-solution-aux next-stack primary-piece-list)))
      )))

(defun search-solution-from-prime-pieces (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
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
       (let* ((frame-piece    (car frame-pieces))
              ;;
              (none-frame-pieces (remove frame-piece primary-pieces :test #'equalp))
              (stack-of-states_t0
                (list (make-fs-from-piece frame-piece none-frame-pieces)))
              (solution-and-paths (search-solution-aux stack-of-states_t0 none-frame-pieces)))
         (mapcar #'(lambda (s) (fs-frame-piece s)) solution-and-paths)
         )))))


#|
;; usage
(search-solution-from-prime-pieces
(cons (car *example-problem-9*) (cdr *example-problem-9*)))
|#

