
(in-package :puzzle-1617)


;;;; filter functions
;;;; congruent, no-future-shape, call filter-functions

;;; detect congruent of piece and piece
;; new implement. faster

(defun detect-rot-list-exists-all-equal-index (rot-list1 rot-list2
                                               &key (test #'equal) (also-reverse nil))
  (labels ((aux (rot1_ab rot1_d)
             (let ((rot1_da (append (cdr rot1_d) (list (car rot1_ab)))))
               (cond ((null (car rot1_ab))           nil)
                     ((every test rot1_da rot-list2) t)
                     (t                              (aux (cdr rot1_ab) rot1_da))))))
    (cond ((and (null rot-list1) (null rot-list2) t))
          ((not (= (length rot-list1) (length rot-list2))) nil)
          (also-reverse
           (or (aux rot-list1 rot-list1)
               (aux (reverse rot-list1) (reverse rot-list1))))
          (t
           (aux rot-list1 rot-list1)))))

(defun detect-piece-exist-congruent-corresponding-index (piece1 piece2)
  ;;
  (let ((shape1 (piece-shape piece1))
        (shape2 (piece-shape piece2)))
    (and (detect-rot-list-exists-all-equal-index
          (shape-segment-length^2-list shape1)
          (shape-segment-length^2-list shape2)
          :test #'ser= :also-reverse t)
         (detect-rot-list-exists-all-equal-index
          (shape-angle-list shape1)
          (shape-angle-list shape2)
          :test #'ser= :also-reverse t))))


;;; detect congruent of piece and piece
;; old implement. slow

#|

(defun detect-piece-point-selection-makes-congruent-transform (select-piece.piece)
  (let ((detect_1_2
          (map-to-combination-selection-piece1.piece2
           #'(lambda (sd_1 tm_1 sd_2 tm_2) ;; Shape_Dush, TransforM
               sd_1 sd_2
               (let* (;; Coordinates[List]
                      (c1 (shape-coord-points sd_1))
                      (c2 (shape-coord-points sd_2))
                      ;; rotated C, where δ1 = δ2
                      (rc1 (re-order-coordinate-points-by-transform tm_1 c1 nil))
                      (rc2 (re-order-coordinate-points-by-transform tm_2 c2 nil)))
                 (every #'vec3-ser= rc1 rc2)))
           select-piece.piece
           :shape-transformer #'transform-shape-by-transformation-matrix-however-nil-approxs)))
    (or (car detect_1_2) (cadr detect_1_2))))

(defun detect-piece-exist-congruent-transform (piece1 piece2)
  ;; if piece1 and piece2 is congruent,
  ;; it exist congruent transform, all coordinates of corresponding
  ;; points at piece1 and piece2 are equal in its specific transform.
  ;;
  ;; in other words,
  ;; whether if the exist of transform which makes perfectly overlapping or not.
  (some #'detect-piece-point-selection-makes-congruent-transform
        (whole-set-of-point-and-edge-selections-piece-piece
         piece1 piece2)))
|#

;;; detect congruent of piece and piece call

(defun detect-piece-congruent (piece1 piece2 &optional (shape-congruent-only-p nil))
  ;; detect piece1 === piece2
  (and
   ;; == pm_sign
   (eq (piece-pm-sign piece1)
       (piece-pm-sign piece2))
   ;; == num edge points
   (= (length (piece-coord-points piece1))
      (length (piece-coord-points piece2)))
   ;; == primary piecese which composes its piece.
   (and (not shape-congruent-only-p)
        (set-equal (mapcar #'piece-id (list-of-primary-piece-list-of-synthesized-piece piece1))
                   (mapcar #'piece-id (list-of-primary-piece-list-of-synthesized-piece piece2))))

   ;; new implement
   (or
    ;;(and (zero-shape-piece-p piece1)
    ;;     (zero-shape-piece-p piece2))
    (detect-piece-exist-congruent-corresponding-index piece1 piece2))

   ;; old implement
   #|
   (or
    ;; root is zero shape
    (and (zero-shape-piece-p piece1)
         (zero-shape-piece-p piece2))
    ;; exist of congruent transform
    (detect-piece-exist-congruent-transform piece1
                                            piece2))
   |#
   ))

;;; no-future-shape

(defun primary-params (synthesized-piece primary-piece-list)
  (labels ((afcl (most-f restriction-constant lis)
             (apply most-f (cons restriction-constant lis))))
    (let* ((minimal-angle
             (afcl #'min *num-divergent*
                   (flatten
                    (mapcar #'(lambda (p)
                                (shape-angle-list (piece-shape p)))
                            primary-piece-list))))
           (objective-length^2
             (cond (;; if subjective is hole[-], minimal line-length of piece[+] is objective
                    (shape-minus-p (piece-shape synthesized-piece))

                    (afcl #'min *num-divergent*
                          (flatten
                           (mapcar #'(lambda (p)
                                       (shape-segment-length^2-list (piece-shape p)))
                                   (remove-if-not #'(lambda (p) (shape-plus-p (piece-shape p)))
                                                  primary-piece-list)))))
                   (;; if subjective is piece[+], maximum line-length of hole[-] is objective
                    (shape-plus-p (piece-shape synthesized-piece))
                    (afcl #'max *num-zero*
                          (flatten
                           (mapcar #'(lambda (p)
                                       (shape-segment-length^2-list (piece-shape p)))
                                   (remove-if-not #'(lambda (p) (shape-minus-p (piece-shape p)))
                                                  primary-piece-list)))))
                   (;; something none Real
                    t
                    (warn "Wrong Sign Piece")
                    ;; (format nil "Wrong Sign Piece: ~A~%" synthesized-piece))
                    nil))))
      `((:minimal-angle . ,minimal-angle)
        (:objective-length^2 . ,objective-length^2))
      )))

(defun no-future-angles-p (angle-list minimal-angle)
  ;; Cut if ∃(angle_n + min_angle > 360°)
  (find-if #'(lambda (a_n)   (ser> (+ a_n minimal-angle) *pi*2*)) angle-list))

(defun no-future-hole-lines-p (hole-length^2-list minimal-length^2)
  ;; Cut if ∃(hole_length_n < min_length)
  (find-if #'(lambda (l^2_n) (ser< l^2_n minimal-length^2)) hole-length^2-list))

(defun no-future-piece-lines-p (piece-length^2-list hole-maximum-length^2)
  ;; Cut if ∃(piece_length_n > hole_max_length)
  (find-if #'(lambda (l^2_n) (ser> l^2_n hole-maximum-length^2)) piece-length^2-list))

(defun no-future-shape-p (shape primary-params)
  (let ((minimal-angle  (assocdr :minimal-angle   primary-params))
        (objective-length^2 (assocdr :objective-length^2 primary-params))
        ;;
        (angle-list (shape-angle-list shape))
        (length^2-list (shape-segment-length^2-list shape)))
    (or
     ;; angle
     (no-future-angles-p angle-list minimal-angle)
     ;; line length
     (cond ((shape-minus-p shape) ;; if subjective is hole [-] ,
            (no-future-hole-lines-p  length^2-list objective-length^2))
           ((shape-plus-p shape)  ;; if subjective is piece[+] ,
            (no-future-piece-lines-p length^2-list objective-length^2) )
           (t                     ;; if none Real sign
            t)))))

(defun detect-no-future-state (state)
  (if (zero-shape-piece-p (fs-frame-piece state))
      nil
      (let ((primary-params (primary-params
                             (fs-frame-piece state)
                             (fs-primary-rest state))))
        (no-future-shape-p (piece-shape (fs-frame-piece state))
                           primary-params))))

#|
;; old implement
(defun detect-no-future-piece (synthesized-piece primary-piece-list)
  (if (zero-shape-piece-p synthesized-piece)
      nil
      (let ((primary-params (primary-params
                             synthesized-piece
                             (list-of-unused-primary-piece-list-of-synthesized-piece
                              synthesized-piece primary-piece-list))))
        (no-future-shape-p (piece-shape synthesized-piece)
                           primary-params))))
|#

;;; detect domains of plus-synthesized-piece[+] overs frame[-]

(defun detect-domain1-overs-domain2 (domain1 domain2)
  (let ((l1_x (abs (- (nth 2 domain1) (nth 0 domain1))))
        (l1_y (abs (- (nth 3 domain1) (nth 1 domain1))))
        (l2_x (abs (- (nth 2 domain2) (nth 0 domain2))))
        (l2_y (abs (- (nth 3 domain2) (nth 1 domain2)))))
    (or (ser> l1_x l2_x)
        (ser> l1_y l2_y))))

(defun detect-domain-of-plus-piece-overs-frame (synthesized-piece frame-piece)
  (cond ((shape-plus-p (piece-shape synthesized-piece))
         ((lambda (piece_shape[+] hole_shape[-])
            (detect-domain1-overs-domain2
             (shape-domain-rect (piece-coord-points piece_shape[+]))
             (shape-domain-rect (piece-coord-points hole_shape[-] ))))
          synthesized-piece
          frame-piece))
        (t nil)))


;;; call filter functions

(defun remove-congruent-from-state-list (fs-list) ;; (state-list)
  (let ((lis fs-list))
    (cond ((null lis) '())
          (t (cons (car lis)
                   (remove-congruent-from-state-list
                    (remove-if #'(lambda (s)
                                   (detect-piece-congruent (fs-frame-piece (car lis))
                                                           (fs-frame-piece s)))
                               (cdr lis))))))))




(defun remove-no-future-state-from-state-list (fs-list) ;; (state-list)
  (remove-if #'(lambda (fs) (detect-no-future-state fs))
             fs-list))




(defun remove-plus-piece-overs-frame-from-state-list (state-list frame-state) ;; (state-list)
  (remove-if #'(lambda (piece-state)
                 (detect-domain-of-plus-piece-overs-frame
                  (fs-frame-piece piece-state) ;; todo: [±]piece is not only frame-piece
                  (fs-frame-piece frame-state)))
             state-list))


(defun cut-function-for-evaluation-value-by-delta-points_sum (fs)
  (let ((n-steps (1- (length (fs-primary-used fs))))
        ;; (n-value (fs-evaluation-value fs))
        (n-d/dt-value (fs-d/dt-evaluation-value fs))
        (step-const 3))
    (if (>= n-steps step-const)
        (< n-d/dt-value 2)
        nil)))

(defun remove-by-evaluation-value
  (fs-list ;; (state-list)
   &optional (cut-function
              #'cut-function-for-evaluation-value-by-delta-points_sum))
  (remove-if cut-function
             fs-list))



;; old implement.      args synthed-piece-list
;; where new implement args state-list

#|
(defun remove-congruent-from-synthesized-piece-list (synthesized-piece-list)
  (let ((lis synthesized-piece-list))
    (cond ((null lis) '())
          (t (cons (car lis)
                   (remove-congruent-from-synthesized-piece-list
                    (remove-if #'(lambda (p)
                                   (detect-piece-congruent (car lis) p))
                               (cdr lis))))))))

(defun remove-no-future-shaped-piece-from-synthesized-piece-list
    (synthesized-piece-list primary-piece-list)
  (remove-if
   #'(lambda (synthed-piece)
       (detect-no-future-piece
        synthed-piece
        ;; todo select: all primary piece-list
        ;; primary-piece-list))
        ;; todo select: resting primary piece-list
        (list-of-unused-primary-piece-list-of-synthesized-piece synthed-piece primary-piece-list)
        ))
   synthesized-piece-list))

(defun remove-plus-piece-overs-frame-from-synthesized-piece-list
    (synthesized-piece-list frame-piece)
  (remove-if #'(lambda (synthed-piece)
                 (detect-domain-of-plus-piece-overs-frame synthed-piece frame-piece))
             synthesized-piece-list))
|#


;;; filter states

(defun filter-fs-list (fs-list state-this-step) ;; (state-list)
  ;; todo: rewrite this into function composition form
  ;; todo: rewrite stack filter for gradient_stack by this function
  ;; call filter functions
  (first-n
   *fs-stackwidth-const*
   (remove-plus-piece-overs-frame-from-state-list
    (remove-no-future-state-from-state-list
     (remove-by-evaluation-value
      (remove-congruent-from-state-list fs-list)))
    ;; todo. this is once old frame. frame of this step is may be better
    state-this-step)))


#|
(defun filter-piece-list-from-synthesized-piece-list
    (state primary-piece-list synthesized-piece-list)
  ;; call filter functions, and sort.
  (remove-plus-piece-overs-frame-from-synthesized-piece-list
   (remove-no-future-shaped-piece-from-synthesized-piece-list
    (remove-congruent-from-synthesized-piece-list synthesized-piece-list)
    primary-piece-list)
   ;; todo. this is once old frame. frame of this step is may be better
   (fs-frame-piece state)))
|#

