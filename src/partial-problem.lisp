(in-package :puzzle-1617)

;;;; partial problem
;;; such as line or angle




;; 1. all of length of frame segment has equall length by the synthesize of other pieces.
;;    ∀(x±1) . (x±1) is frame_segment
;;    => ∃N . Length((x±1)) = Σ Length(piece_segment_(ni±1)) while N={n1,n2,...,nn}
;; 2. all angle of all points has 360-degree synthesize with other pieces.
;;    ∀x. x is piece_point
;;    => ∃N. Σ Angle(piece_angle_ni) while N={n1,n2,...,nn} = 2*π


;; 部分問題を先に解く
;; 部分問題を解けない頂点や辺を起点とする合成は行わないことで、計算を削減する。

;;; defines and initialize


;;;

(defparameter *memo-of-partial-problem*
  nil)

(defparameter *depth-const-of-partial-problem*
  3)

(defstruct (value-and-sy-param (:constructor make-vvsy) (:conc-name vvsy-))
  (:angle 1000000) (:length^2 1000000) ;; value value
  (:id -1) (:nc 0) (:pm 0)) ;; sy-select parameter

(defun partial-value-sy-param-list (piece)
  (let ((id (piece-id piece))
        (sy-param-list
          (whole-set-of-point-and-edge-selections-piece piece)))
    (mapcar
     #'(lambda (sy-param) ;; [nth_point, pm_sign]
         (let* ((nc (nth 0 sy-param)) (pm (nth 1 sy-param))
                (angle    (modnth (+ nc 0)
                                  (shape-angle-list (piece-shape piece))))
                (length^2 (modnth (+ nc (if (eq pm +1) 0 -1))
                                  (shape-segment-length^2-list (piece-shape piece)))))
           (make-vvsy :angle angle :length^2 length^2 :id id :nc nc :pm pm)))
     sy-param-list)))
  
(defun sy-select-parameters-from-piece-list (piece-list)
  (flatten (mapcar #'partial-value-sy-param-list piece-list)))


;;;

(defun solve-partial-problem-aux (objective-value choice-value-list
                                  this-depth-queue next-depth-queue solutions current-depth)
  (cond ((>= current-depth *depth-const-of-partial-problem*) ;; end
         solutions)
        ((null this-depth-queue) ;; next depth
         (solve-partial-problem-aux objective-value choice-value-list
                                    next-depth-queue '() solutions (+ 1 current-depth)))
        (t ;; forward and next queue at this depth.
         (let* ((step-vs (car this-depth-queue))
                (step-next-depth-queue-combination
                  (mapcar #'(lambda (v) (cons v step-vs)) ;; it is unneeded to remove equally set
                          choice-value-list))
                (step-next-depth-queue
                  (remove-if-not #'(lambda (vs) (ser<= (reduce #'+ vs :initial-value 0)
                                                       objective-value))
                                 step-next-depth-queue-combination))
                (step-solutions
                  (remove-if-not #'(lambda (vs) (ser= (reduce #'+ vs :initial-value 0)
                                                      objective-value))
                                 step-next-depth-queue-combination)))
           #|
           (format t "depth ~A. ~A = Sum ,~A~%"
                   current-depth
                   (reduce #'+ step-vs :initial-value 0)
                   step-vs)
           |#
           (solve-partial-problem-aux
            objective-value choice-value-list
            (cdr this-depth-queue)
            (append next-depth-queue step-next-depth-queue)
            (append solutions step-solutions)
            current-depth)))))


                                    
(defun solve-partial-problem (objective-value choice-value-list)
  (let* ((t0_combination
          (mapcar #'list choice-value-list))
         (t1_queue
           (remove-if-not #'(lambda (vs) (ser<= (nth 0 vs) objective-value)) t0_combination))
         (t1_solutions
           (remove-if-not #'(lambda (vs) (ser=  (nth 0 vs) objective-value)) t0_combination)))
    (remove-equally-set-from-set-list
     (solve-partial-problem-aux objective-value choice-value-list
                                t1_queue '()
                                t1_solutions 1) ;; current_depth(t1) = 1
     :test #'ser=)))

(defun solve-partial-angle (vvsy available-piece-list)
  (solve-partial-problem
   (- *pi*2* (vvsy-angle vvsy))
   (flatten (mapcar #'piece-angle-list available-piece-list))))


;;;
