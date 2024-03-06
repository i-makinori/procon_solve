
(in-package :puzzle-1617)


;;;; evaluation functions

;;; sort by delta points

(defun sort-by-delta_points_sum (synthesized-piece-list)
  (sort synthesized-piece-list
        #'(lambda (p1 p2)
            (> (delta_points-of-synthesize_sum p1)
               (delta_points-of-synthesize_sum p2)))))

(defun delta_points-of-synthesize_sum (synthesized-piece)
  (let ((n-points-of-primary
         (apply #'+ (mapcar #'(lambda (piece) (length (piece-coord-points piece)))
                     (list-of-primary-piece-list-of-synthesized-piece synthesized-piece))))
        (n-points-of-synthesized-piece
          (length (piece-coord-points synthesized-piece))))
    (- n-points-of-primary n-points-of-synthesized-piece)))


(defun sort-by-delta_points_delta (synthesized-piece-list)
  (sort synthesized-piece-list
        #'(lambda (p1 p2)
            (> (delta_points-of-synthesize_delta p1)
               (delta_points-of-synthesize_delta p2)))))

(defun delta_points-of-synthesize_delta (synthesized-piece)
  (let* ((piece_root synthesized-piece)
         (t1 (piece-transform1 piece_root))
         (t2 (piece-transform2 piece_root)))
    (if (or (null t1) (null t2))
        0
        (let* ((piece_t1 (transform-piece t1))
               (piece_t2 (transform-piece t2)))
          (if (or (null (piece-coord-points piece_t1))
                  (null (piece-coord-points piece_t2)))
              0
              (+ 
               ;; body term
               (+ (length (piece-coord-points piece_t1)))
               (+ (length (piece-coord-points piece_t1)))
               (- (length (piece-coord-points piece_root)))
               ;; fix by step
               ;;(+ (/ 1 (piece-id piece_root)))
                     ))))))


;;; evaluation functions for evaluate values

(defun evaluation-value-by-delta-points_sum (state primary-piece-list)
  primary-piece-list
  (delta_points-of-synthesize_sum (fs-frame-piece state)))

(defun evaluation-value-by-delta-points_delta (state primary-piece-list)
  primary-piece-list
  (delta_points-of-synthesize_delta (fs-frame-piece state)))

(defun evaluation-value-by-div1-nomials (state primary-piece-list)
  ;;primaries
  primary-piece-list
  state
  0)

(defun sorted-states-by-evaluation-function (state-list-new state-list-resting)
  ;; todo: merge sort. because states rest are sorted and values stored in logically.
  (sort
   ;; sort is unneeded if sorted before
   (append state-list-new state-list-resting)
   #'(lambda (state1 state2)
       (> (fs-evaluation-value state1)
          (fs-evaluation-value state2)))))


;;; differential (d/dt) of evaluation value

(defun d/dt-evaluation-value (evaluation-value_new evaluation-value_previous &optional (delta_t 1))
  (if (every #'numberp (list evaluation-value_new evaluation-value_previous))
      (/ (- evaluation-value_new evaluation-value_previous) delta_t)
      0))
