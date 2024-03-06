
(in-package :puzzle-1617)

;;;; Search in Beam method.
;; 
;; # about Beam Search method.
;; n of Beams searches different branches each other parallely (in logically).
;;

;;; parameters and variables

(defparameter *beam-width* 3) ;; parameter
(defparameter *beam-current-index* 0) ;; variable


;;; structure

(defstruct beam
  ;; statement of beam
  (:index nil)
  (:depth 0)
  ;; (:solution-p nil)
  (:stack nil))


;;; functions for Beam Search

(defun beam-next (beam-previous next-stack-by-previous)
  ;; (make-beam :index :depth :stack)
  ;;
  #|
  (setf (beam-index beam-previous!)
        (beam-index beam-previous!))
  (setf (beam-depth beam-previous!)
        (1+ (beam-depth beam-previous!)))
  (setf (beam-stack beam-previous!)
        (cdr (beam-stack beam-previous!)))
  beam-previous!)
|#
  (make-beam :index (beam-index beam-previous)
             :depth (1+ (beam-depth beam-previous))
             :stack next-stack-by-previous))

#|
(defun restart-beam (state-stack)
  (incf *beam-current-index*)
  (let ((restarted-beam
          (make-beam :index *beam-current-index*
                     :depth 0
                     :stack (nth 0 state-stack))))
    ;; todo reset evaluate value of its gradient

    ;;(setf (aref (fs-evaluation-value (nth 0 state-stack)))
    ;;(recalc-evaluation-value (aref (fs-evaluation-value (nth 0 state-stack)))))
    ;; (indesrtsort (car state-stack) (cdr state-stack))
    restarted-beam
))
|#

(defun select-head-n-of-stack-into-n-beams (n next-stack-by-previous beam-previous)
  ;; todo: fix the selection of next state(stack_n) take from wide area of tree.
  (mapcar
   #'(lambda (n) n
       (let ((next-index (if (> n 0)
                             (progn (incf *beam-current-index*)
                                    *beam-current-index*)
                             (beam-index beam-previous))))
         (make-beam :index next-index
                    :depth (1+ (beam-depth beam-previous))
                    ;; todo: incorrect restart node
                    ;;:stack (nthcdr n next-stack-by-previous))))
                    :stack (list (nth n next-stack-by-previous)))))
   (remove-if #'(lambda (n) 
                  (null (nthcdr n next-stack-by-previous))) ;; no future
              (from-m-to-n-list 0 (- n 1)))))



;;; Beam Search

;; minus to frame method

(defun search-solution-aux-beam (beam-queue primary-piece-list)
  (let* (;; 
         (beam-of-this-step (car beam-queue))
         (rest-queue (cdr beam-queue))
         ;;
         (state-of-this-step (car (beam-stack beam-of-this-step)))
         (stacking-of-this-step (cdr (beam-stack beam-of-this-step))))
    ;; format before once list 
    (format-search-status-before state-of-this-step primary-piece-list)
    (format t "d/dt: ~A~%" (fs-d/dt-evaluation-value state-of-this-step))
    (let* ((states-of-next-step (states-of-next-step-from-1-state state-of-this-step
                                                                  primary-piece-list))
           (next-stack-of-this-step (next-state-stack states-of-next-step stacking-of-this-step))
           ;;
           (next-queues-by-this-step
             (select-head-n-of-stack-into-n-beams
              (- *beam-width* (length beam-queue) -1) ;; -1 is this-step
              next-stack-of-this-step
              beam-of-this-step))
           
           (next-queue (append rest-queue next-queues-by-this-step)))
      ;; format after once list
      (format-search-status-after next-stack-of-this-step)
      (format t "beam {type, depth}: {~A, ~A}~%"
              (beam-index beam-of-this-step)
              (beam-depth beam-of-this-step))
      ;; HTML
      (write-piece-list-as-html 
       (mapcar #'(lambda (state) (fs-frame-piece state)) next-stack-of-this-step))
      ;;(incf *n-search-iter*)
      (cond ((null next-stack-of-this-step) ;; no methods in this beam's stack
             (format t "=== the Beam_n is Cut By Dead Twig ===~%")
             (search-solution-aux-beam rest-queue primary-piece-list))
            ((state-is-solution-p (car next-stack-of-this-step))
             ;; car is solution, however return all
             next-stack-of-this-step
             )
            ;; todo: cut by n-count and restart again
            ;;((> *n-search-iter* *n-search-iter-max*)
            ;;(format t "could not get solutions in ~A trial.~%" *n-search-iter*)
            ;;nil)
            (t ;; search-next
             (search-solution-aux-beam next-queue primary-piece-list))))))

(defun search-solution-from-prime-pieces-beam (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
    (setf *n-search-iter* 0)
    (setf *beam-current-index* 0)
    (cond
      ((not (= 1 (length frame-pieces)))
       (warn (format nil "whole-piece-list has multiple frames. IDs: ~A~%"
                     (mapcar #'piece-id frame-pieces)))
       nil)
      (t 
       (let* ((frame-piece    (car frame-pieces))
              ;;
              (none-frame-pieces
                (list-of-unused-primary-piece-list-of-synthesized-piece frame-piece primary-pieces))
              (stack0 (list (make-fs-from-piece frame-piece none-frame-pieces)))
              ;;
              (stack-of-states_t0
                (list (make-beam :index 0
                                 :stack stack0)))
              (solution-and-paths (search-solution-aux-beam
                                   stack-of-states_t0 none-frame-pieces)))
         (mapcar #'(lambda (s) (fs-frame-piece s)) solution-and-paths)
         )))))
