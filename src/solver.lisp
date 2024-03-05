
(in-package :puzzle-1617)

;;;; solver


;;;; search solution

;; minus to frame method







;;; search



;;; 



(defparameter *grad-stack-width-const* 1000 "max stack width for search of its score memo(grad)")



;;; Beam Search with "cutten access to gradient stack"

;; insert , state stack

(defun insert (thing into &key (older-function #'<))
  "if you use insert for the as sort, into need to be sorted"
  (labels ((aux (rest memo)
             (cond 
               ((or (null rest)
                    (funcall older-function thing (car rest)))
                (append (reverse (cons thing memo)) rest))
               (t 
                (aux (cdr rest) (cons (car rest) memo))))))
    (aux into '())))


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

;; beam 


(defparameter *beam-width* 3)
(defparameter *beam-current-index* 0)

(defstruct beam
  ;; statement of beam, used for beam search.
  (:index nil)
  (:depth 0)
  ;; (:solution-p nil)
  (:stack nil))

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
  ))|#

(defun select-head-n-of-stack-into-n-beams (n next-stack-by-previous beam-previous)
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

(defun fs-decrease-d/dt-evaluation-by-retake (fs)
  "retake its state, gradient (d/dt) of evaluation value is decreased"
  ;; todo: convergence sequence 
  ;; lim (n->infinity) => (f(n)->C) , C such as +2
  (setf (fs-d/dt-evaluation-value fs)
        ((lambda (v)
           (* v (/ 1 (+ 1 0.05)))) ;; V_next = V_now * 1 / (1+ε)
         (fs-d/dt-evaluation-value fs)))
  fs)


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
    

;;; Beam Search

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
      (cond ((null next-stack-of-this-step) ;; no methods
             (format t "there is no solutions. IDs: ~A~%" (mapcar #'piece-id primary-piece-list))
             nil)
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

