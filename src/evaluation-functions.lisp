
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

;;; sort

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


;;; evaluation functions for evaluate values

;; method I

(defun evaluation-value-by-delta-points_sum (state primary-piece-list)
  primary-piece-list
  (delta_points-of-synthesize_sum (fs-frame-piece state)))

(defun evaluation-value-by-delta-points_delta (state primary-piece-list)
  primary-piece-list
  (delta_points-of-synthesize_delta (fs-frame-piece state)))

;; method II

(defun evaluation-value-by-remain-edges (state primary-piece-list)
  ;;primaries
  (let* ((frame-piece (fs-frame-piece state))
         (frame-edges (length (piece-coord-points frame-piece)))
         (remain-pieces (list-of-unused-primary-piece-list-of-synthesized-piece
                         frame-piece primary-piece-list))
         (remain-primes-edges (apply #'+ (mapcar #'(lambda (p) (length (piece-coord-points p)))
                                                 remain-pieces)))
         (remain-edges (+ frame-edges remain-primes-edges)))
    (- (* 50 50) remain-edges)))


;; method III

(defun piece-apply-recursive (func piece 
                              &key
                                (bottom #'(lambda (piece trm) piece trm nil))
                                (transformation-matrixes-reversed (list *identity-matrix-3x3*)))
  (cond ((primary-piece-p piece)
         (funcall bottom
                  piece
                  (reduce #'matrix3x3-product (reverse transformation-matrixes-reversed))))
        ((and (piece-p (transform-piece (piece-transform1 piece)))
              (piece-p (transform-piece (piece-transform2 piece))))
         (funcall func
                  (piece-apply-recursive
                   func
                   (transform-piece (piece-transform1 piece))
                   :bottom bottom
                   :transformation-matrixes-reversed
                   (cons (transform-transformation-matrix (piece-transform1 piece))
                         transformation-matrixes-reversed))
                  
                  (piece-apply-recursive
                   func
                   (transform-piece (piece-transform2 piece))
                   :bottom bottom
                   :transformation-matrixes-reversed
                   (cons (transform-transformation-matrix (piece-transform2 piece))
                         transformation-matrixes-reversed))))
        (t (warn "something srong for piece apply-recursive.")
           nil)))

#|
(defun f-synthesized-*-of-piece 
    (listing-function &optional
                        (testing-function #'ser=)
                        (bottom-function #'(lambda (p tr) tr (funcall listing-function p))))
  (lambda (piece)
    (let* ((list-of-root-*
             (funcall listing-function piece)
           (list-of-all-*
             (piece-apply-recursive #'append piece
                                    :bottom bottom-function))
           (synthesized-angles
             (remove-if #'(lambda (cpa_n) (member cpa_n list-of-root-* :test testing-function))
                        list-of-all-*)))
      (remove-duplicates synthesized-angles :test testing-function))))


(defun synthesized-angles-of-piece (piece)
  (funcall (f-synthesized-*-of-piece #'piece-angle-list #'ser=)
           piece))

(defun synthesized-coord-points-of-piece (piece)
  (funcall (f-synthesized-*-of-piece
            ;;#'piece-coord-points
            #'(lambda (p tr)
                (shape-coord-points
                 (transform-shape-by-transformation-matrix (piece-shape p) tr))))
           #'vec3-ser=
            #'(lambda (p tr)
                (shape-coord-points
                 (transform-shape-by-transformation-matrix (piece-shape p) tr))))
           piece))

(defun synthesized-segments-of-piece (piece)
  (funcall (f-synthesized-*-of-piece #'piece-
  )

(defun evaluation-value-by-partial-synthesized_delta (delta primary-piece-list)
  
  )

(defun evaluation-value-by-partial-synthesized_sum (state primary-piece-list)
  
  )
|#
