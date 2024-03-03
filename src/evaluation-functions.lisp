
(in-package :puzzle-1617)


;;;; evaluation functions

;;; sort by delta points

(defun sort-by-delta_points (synthesized-piece-list)
  (sort synthesized-piece-list
        #'(lambda (p1 p2)
            (> (delta_points-of-synthesize p1)
               (delta_points-of-synthesize p2)))))

(defun delta_points-of-synthesize (synthesized-piece)
  (let ((n-points-of-primary
         (apply #'+ (mapcar #'(lambda (piece) (length (piece-coord-points piece)))
                     (list-of-primary-piece-list-of-synthesized-piece synthesized-piece))))
        (n-points-of-synthesized-piece
          (length (piece-coord-points synthesized-piece))))
    (- n-points-of-primary n-points-of-synthesized-piece)))


;;; evaluation functions for evaluate values

(defun evaluation-value-by-delta-points (state primary-piece-list)
  primary-piece-list
  (delta_points-of-synthesize (assocdr :frame state)))

(defun evaluation-value-by-div1-nomials (state primary-piece-list)
  ;;(let* ((synth-prims (list-of-primary-piece-list-of-synthesized-piece (assocdr :frame state))))
  ;;primaries
  primary-piece-list
  state
  0)

(defun sorted-states-by-evaluation-function (evaluation-function state-list! primary-piece-list)
  (let ((states-added-evaluation-value
          (mapcar #'(lambda (state)
                      (if (null (assocdr :evaluation-value state))
                          (cons `(:evaluation-value 
                                  . ,(funcall evaluation-function state primary-piece-list))
                                state)
                          state))
                  state-list!)))
    (sort
     states-added-evaluation-value
     #'(lambda (state1 state2)
         (> (assocdr :evaluation-value state1)
            (assocdr :evaluation-value state2))))))
