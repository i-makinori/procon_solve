
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
  ;; proceed beam
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

(defun generate-initial-beams (n initial-state-stack)
  (mapcar
   #'(lambda (n)
       (incf *beam-current-index*)
       (make-beam :index n
                  :depth 0
                  :stack (list (nth n initial-state-stack))))
   (remove-if #'(lambda (n)
                  (null (nth n initial-state-stack)))
              (from-m-to-n-list 0 (- n 1)))))


;;; Beam Search

;; minus to frame method

(defun search-solution-aux-beam (beam-queue primary-piece-list)
  (cond
    ((null beam-queue) ;; bottom of search
     (format t "search end, could not get solutions.~%")
     nil)
    (t ;; continue search
     (let* (;;
            (beam-of-this-step (car beam-queue))
            (rest-queue (cdr beam-queue))
            ;;
            (state-of-this-step (car (beam-stack beam-of-this-step)))
            (stacking-of-this-step (cdr (beam-stack beam-of-this-step))))
       ;; format before once list
       (format-search-status-before state-of-this-step primary-piece-list)
       ;; (format t "d/dt: ~A~%" (fs-d/dt-evaluation-value state-of-this-step))
       (let* ((states-of-next-step (states-of-next-step-from-1-state state-of-this-step
                                                                     primary-piece-list))
              (next-stack-by-this-step (next-state-stack states-of-next-step stacking-of-this-step))
              (next-beam-of-this-step (beam-next beam-of-this-step next-stack-by-this-step))
              ;;
              (next-queue (append rest-queue (list next-beam-of-this-step))))
         ;; format after once list
         (format-search-status-after next-stack-by-this-step)
         (format t "beam {type, depth}: {~A, ~A}~%"
                 (beam-index beam-of-this-step)
                 (beam-depth beam-of-this-step))
         ;; HTML
         (write-piece-list-as-html
          (mapcar #'(lambda (state) (fs-frame-piece state)) next-stack-by-this-step))
         ;;(incf *n-search-iter*)
         (cond ((null next-stack-by-this-step) ;; no methods in this beam's stack
                (format t "=== the Beam_n is Cut By Dead Twig ===~%")
                (search-solution-aux-beam rest-queue primary-piece-list))
               ((state-is-solution-p (car next-stack-by-this-step))
                ;; car is solution, however return all
                next-stack-by-this-step)
               ((>= (beam-depth beam-of-this-step) *n-search-iter-max*)
                (format t "=== the beam_n is Cut By iter-max ===~%")
                (search-solution-aux-beam rest-queue primary-piece-list))
               (t ;; search-next
                (search-solution-aux-beam next-queue primary-piece-list))))))))


(defun search-solution-from-prime-pieces-beam (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
    ;;
    (init-meta-params primary-pieces
                      :iter-max 100
                      :stack-width-const-1 100
                      :beam-width 6)
    ;;
    (cond
      ((not (= 1 (length frame-pieces)))
       (warn (format nil "whole-piece-list has multiple frames. IDs: ~A~%"
                     (mapcar #'piece-id frame-pieces)))
       nil)
      (t
       (let* (;; frame and primes
              (frame-piece    (car frame-pieces))
              (none-frame-pieces
                (list-of-unused-primary-piece-list-of-synthesized-piece frame-piece primary-pieces))
              ;; t0
              (stack_t0 (list (make-fs-from-piece frame-piece none-frame-pieces)))
              ;; t1
              (stack_t1 (next-state-stack
                         (states-of-next-step-from-1-state (car stack_t0) none-frame-pieces)
                         '()))
              ;; beams by t1
              (beam-queue_t1 ;; first beam queue
                (generate-initial-beams *beam-width* stack_t1))
              ;; search solution in tree by beam
              (solution-and-paths
                (search-solution-aux-beam beam-queue_t1 none-frame-pieces)))
         (mapcar #'(lambda (s) (fs-frame-piece s)) solution-and-paths)
         )))))
