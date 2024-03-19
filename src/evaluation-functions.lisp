
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


(defun evaluation-value-by-remain-edges-rest-better-synthesize (state primary-piece-list)
  ;; even worst piece-list , there is more than one of -2 pointed synthesizes.
  (let* ((nth-synthesize
           (- (length (list-of-primary-piece-list-of-synthesized-piece (fs-frame-piece state)))
              1))
         ;;(fix-by-synthesizes (- (* 2.00 nth-synthesize)))) ;; -2 is 0
         (fix-by-synthesizes (- (* (- 2.00 1.00) nth-synthesize)))) ;; -2 is 0
    (+ (evaluation-value-by-remain-edges state primary-piece-list)
       fix-by-synthesizes)))


(defun frame-piece-before-k-step (frame-piece k)
  (cond ((or (<= k 0) (primary-piece-p frame-piece))
         frame-piece)
        (t
         (frame-piece-before-k-step
          (transform-piece (piece-transform1 frame-piece))
          (- k 1)))))

(defun evaluation-value-by-remain-edges-in-reduce-in-k-step (state-of-frame primary-piece-list)
  (let* ((k 3) ;; k-step
         (frame-piece-k-step-before
           (frame-piece-before-k-step (fs-frame-piece state-of-frame) k))
         (state-of-k-step-before
           ;;(make-fs-from-piece frame-piece-k-step-before primary-piece-list))
           (make-fs :frame-piece frame-piece-k-step-before
                    :primary-used (list-of-primary-piece-list-of-synthesized-piece
                                   frame-piece-k-step-before)
                    :primary-rest (list-of-unused-primary-piece-list-of-synthesized-piece
                                   frame-piece-k-step-before primary-piece-list)))
         ;;
         (evaluation-value_t0
           (evaluation-value-by-remain-edges state-of-frame         primary-piece-list))
         (evaluation-value_t0-k
           (evaluation-value-by-remain-edges state-of-k-step-before primary-piece-list))
         )
    ;;(format t "piece-id: ~A~%" (piece-id frame-before-k-step))
    (+ (* 2.1 (evaluation-value-by-num-used-pieces state-of-frame primary-piece-list))
       (- evaluation-value_t0 evaluation-value_t0-k))))


;; method III

(defun evaluation-value-by-num-used-pieces (state primary-piece-list)
  (declare (ignore primary-piece-list))
  (* (length (list-of-primary-piece-list-of-synthesized-piece (fs-frame-piece state)))
     1))


;; method IV

#|
(defun evaluation-value-of-edge (angles unused-primary-piece-list)
  ;; todo refer end-state of partial problem
  ;;(eq (dict-item-end-state angle-dict-item) 'all-paterns
  (let* ((piece-angle-list
           (flatten (mapcar #'piece-angle-list unused-primary-piece-list)))
         (len-angles
           (length (remove-duplicates angles :test #'ser=)))
         (len-fulfill-angles
           (length (remove-if-not 
                    #'(lambda (angle_n)
                        (member angle_n angles :test #'ser=))
                    piece-angle-list))))
    (cond ((zerop len-fulfill-angles)
           +10) ;; todo
          (t
           ;;(/ len-angles len-fulfill-angles)))))
           (+ len-fulfill-angles)))))

(defun evaluation-value-of-line-segment (segment-length^2-list unused-primary-piece-list)
  ;; todo refer end-state of partial problem
  ;;(eq (dict-item-end-state angle-dict-item) 'all-paterns
  (let* ((piece-segment-length^2-list
           (flatten (mapcar #'piece-segment-length^2-list unused-primary-piece-list)))
         (len-segments
           (length (remove-duplicates segment-length^2-list :test #'vec3-ser=)))
         (len-fulfill-segments
           (length (remove-if-not 
                    #'(lambda (length^2_n)
                        (member length^2_n segment-length^2-list :test #'vec3-ser=))
                    piece-segment-length^2-list))))
    (cond ((zerop len-fulfill-segments) 
           +10) ;; todo
          (t
           ;;(/ len-segments len-fulfill-segments)))))
           (+ len-fulfill-segments)))))

(defun evaluation-value-by-remain-edge-combinations (state primary-piece-list)
  ;; segment and length^2
  primary-piece-list
  (let* ((frame-piece (fs-frame-piece state))
         (unused-pieces
           (list-of-unused-primary-piece-list-of-synthesized-piece frame-piece primary-piece-list))
         ;;
         (partial-angle-solutions
           (mapcar #'(lambda (angle)
                       ;;(multiple-value-bind (sets end-status)
                       (solve-partial-angle-with-memo
                        (make-vvsy :angle angle) primary-piece-list
                        *partial-angle-dictionary*))
                   (piece-angle-list frame-piece)))
         ;;
         (partial-length^2-solutions
           (mapcar #'(lambda (length^2)
                       (solve-partial-angle-with-memo
                        (make-vvsy :length^2 length^2) primary-piece-list
                        *partial-length^2-dictionary*))
                   (piece-segment-length^2-list frame-piece)))
         ;; evaluation of each edges (by angle)
         (eval-values-of-each-edges
           (mapcar #'(lambda (angle-sets)
                       (evaluation-value-of-edge (flatten angle-sets) unused-pieces))
                   partial-angle-solutions))
         ;; evaluation of each segments
         (eval-values-of-length^2-solutions
           (mapcar #'(lambda (length^2-sets)
                       (evaluation-value-of-line-segment (flatten length^2-sets) unused-pieces))
                   partial-length^2-solutions))
         )
  
    (cond ((or (member nil eval-values-of-each-edges)
               (member nil eval-values-of-length^2-solutions))
           -1)
          (t
           (+ (evaluation-value-by-remain-edges state primary-piece-list)
              (- (* 1/3 (+ (apply #'+ eval-values-of-each-edges)
                           (apply #'+ eval-values-of-length^2-solutions)))))
              
  ))))

|#


;; method V

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



