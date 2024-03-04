
(in-package :puzzle-1617)

;;;; solver


;;;; search solution

;; minus to frame method

#|
;; usage
(search-solution-from-prime-pieces
 (cons (car *example-problem-9*) (cdr *example-problem-9*)))
|#

(defun format-search-status-before (state-of-this-step primary-piece-list)
  primary-piece-list
  (let* ((piece-frame (assocdr :frame state-of-this-step))
         (primary-piece-using (list-of-primary-piece-list-of-synthesized-piece piece-frame)))
    (format t "~%============~%")
    (format t "synth-list to: ~A,~%"
            (piece-id piece-frame))
    (format t "using-pieces [len]: ~A[~A]~%"
            (mapcar #'piece-id primary-piece-using)
            (length primary-piece-using))))

(defun filter-piece-list-from-synthesized-piece-list
    (state primary-piece-list synthesized-piece-list)
  ;; call filter functions, and sort.
  (remove-plus-piece-overs-frame-from-synthesized-piece-list
   (remove-no-future-shaped-piece-from-synthesized-piece-list
    (remove-congruent-from-synthesized-piece-list synthesized-piece-list)
    primary-piece-list)
   ;; todo. this is once old frame. frame of this step is may be better
   (assocdr :frame state)))

(defun states-of-next-step-from-1-state (state primary-piece-list)
  (let* (;; Synthesized Piece List
         (spl-all-combinations
           ;; todo: select function for combination of step
           ;;(all-synthesizeable-patterns-of-pieces-to-frame
           (all-synthesizeables-of-pieces-to-piece_del-if-e-jam-edge
            (assocdr :frame state)
            (list-of-unused-primary-piece-list-of-synthesized-piece (assocdr :frame state) 
                                                                    primary-piece-list)))
         (spl-filtered ;; patterns-of-step
           (filter-piece-list-from-synthesized-piece-list
            state primary-piece-list spl-all-combinations))
         ;; stack of states
         (states-of-next-step (mapcar #'(lambda (next-frame)
                                          `((:frame . ,next-frame)))
                                      spl-filtered)))
    states-of-next-step))


(defun format-search-status-after (next-stack)
  next-stack
  (format t "EvalValues:")
  (mapcar #'(lambda (s) (format t " ~,4f" (assocdr :evaluation-value s)))
          (first-n 20 next-stack))
  (format t "~%")
  nil)

#|
;; memo
(let ((piece-frame         (assocdr :frame state))
      (primary-piece-using (list-of-primary-piece-list-of-synthesized-piece piece-frame))
      (primary-piece-rests (list-of-unused-primary-piece-list-of-synthesized-piece 
                            piece-frame primary-piece-list))))
|#

(defun next-state-stack (states-new states-rest primary-piece-list)
  ;; todo: merge sort. because states rest are sorted and values stored in logically.
  (sorted-states-by-evaluation-function ;; sort is unneeded if sorted before
   ;;#'evaluation-value-by-delta-points_sum
   #'evaluation-value-by-delta-points_delta
   (append states-new states-rest)
   primary-piece-list)

)

;;; greede DFS

(defun search-solution-aux (stack-of-states primary-piece-list)
  (let* ((state (car stack-of-states))
         (stacking (cdr stack-of-states)))
    ;; format before once list 
    (format-search-status-before state primary-piece-list)
    (let* ((states-of-next-step (states-of-next-step-from-1-state state primary-piece-list))
           (next-stack (next-state-stack states-of-next-step stacking primary-piece-list)))
      ;; format after once list
      (format-search-status-after next-stack)
      ;; HTML
      (write-piece-list-as-html 
       (mapcar #'(lambda (state) (assocdr :frame state)) next-stack))
      ;; finish or recursive
      (cond ((null next-stack) ;; no methods
             (format t "there is no solutions. IDs: ~A~%" (mapcar #'piece-id primary-piece-list))
             nil)
            ((zero-shape-piece-p (assocdr :frame (car next-stack)))
             ;; car is solution, however return all
             next-stack)
            (t ;; search-next
             (search-solution-aux next-stack primary-piece-list)))
      )))


(defun search-solution-from-prime-pieces (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
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
                (list `((:frame . ,frame-piece))))
              (solution-and-paths (search-solution-aux stack-of-states_t0 none-frame-pieces)))
         (mapcar #'(lambda (s) (assocdr :frame s)) solution-and-paths)
)))))



;;;; BFS (delta_edge > 2)

(defparameter *state1*
  `(;; list of state
    (:evaluation-of-edge 0)
    (:piece (piece))))

;;(defun bfs-search-solution-aux (state-priority-queue primary-piece-list)
;;(let* (next-)
;;(cond (

#|
(defun bfs-search-solution-aux (stack-of-states primary-piece-list)
  (let* ((state (car stack-of-states))
         (stacking (cdr stack-of-states)))
    ;; format before once list 
    (format-search-status-before state primary-piece-list)
    (let* ((states-of-next-step (states-of-next-step-from-1-state state primary-piece-list))
           (next-stack (next-state-stack states-of-next-step stacking primary-piece-list)))
      ;; format after once list
      (format-search-status-after next-stack)
      ;; HTML
      (write-piece-list-as-html 
       (mapcar #'(lambda (state) (assocdr :frame state)) next-stack))
      ;; finish or recursive
      (cond ((null next-stack) ;; no methods
             (format t "there is no solutions. IDs: ~A~%" (mapcar #'piece-id primary-piece-list))
             nil)
            ((zero-shape-piece-p (assocdr :frame (car next-stack)))
             ;; car is solution, however return all
             next-stack)
            (t ;; search-next
             (bfs-search-solution-aux next-stack primary-piece-list)))
      )))



(defun bfs-search-solution-from-prime-pieces (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
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
                (list `((:frame . ,frame-piece))))
              (solution-and-paths (bfs-search-solution-aux stack-of-states_t0 none-frame-pieces)))
         (mapcar #'(lambda (s) (assocdr :frame s)) solution-and-paths)
         )))))
|#
