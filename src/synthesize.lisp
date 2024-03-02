(in-package :puzzle-1617)


;;;; Synthesize (Common) piece with (Common) piece

;;; group set of points selections for synthesize

(defun whole-set-of-point-and-edge-selections-piece-piece (piece1 piece2)
  (flatten
   (apply #'append
          (mapcar
           #'(lambda (cp12) ;; cp: coordinate points 1,2
               (list
                (mapcar #'(lambda (sign)
                            `((:p1 . ,piece1) (:n1 . ,(car  cp12)) (:pm1 . ,(car sign))
                              (:p2 . ,piece2) (:n2 . ,(cadr cp12)) (:pm2 . ,(cdr sign))))
                        '((+1 . +1) (+1 . -1) (-1 . +1) (-1 . -1)))))
           (cartesian ;; A x B of both {0,1, ... , (n_points-1)}.
            (from-m-to-n-list 0 (1- (length (piece-points piece1))))
            (from-m-to-n-list 0 (1- (length (piece-points piece2)))))))))


;;; transform by point selection

(defun transform-matrix-{+P2.ME.R.-P1} (p1 v1 p2 v2 mirrorp)
  "p1, p2  : point to start overlapping.
 v1, v2  : edge to overlap. delta of p1, p2 to next points
 mirrorp : mirror?"
  (let* ((I *identity-matrix-3x3*) ;; Identity
         (-P1 (transform-parallel-move (vec3-inverse-xy p1))) ;; move p1 to O(Origin) point
         (R  (transform-rotate (angle *vec3-zero-xy* v1 v2))) ;; rotate from (p1=O)
         (ME (if mirrorp ;; Empty or Mirror by Vector(O,v2) plate
                 (transform-mirror-by-axis-vec v2)
                 *identity-matrix-3x3*))
         (+P2 (transform-parallel-move (identity p2)))) ;; move (p1=O) to p2
    (reduce #'matrix3x3-product
            (list +P2 ME R -P1 I)
            :from-end t)))

(defun make-transforms-by-point-and-edge-selection-parameters
    (transformp point-from1 direction1 piece1
     &optional point-from2 direction2 piece2)
  ;; side which receives another => no transforms
  ;; side which sends to another => transforms
  (labels ((gen-transform (matrix)
             (transform :function-sign *s-add*
                        :point-from point-from1
                        :direction direction1
                        :piece piece1
                        :transformation-matrix matrix)))
    ;;(format t "make-transform: (id, point, direction):~%~A, ~A, ~A, ~%~A, ~A, ~A~%"
    ;;  (piece-id piece1) point-from1 direction1 
    ;;  (piece-id piece2) point-from2 direction2)
    (if (null transformp)
        (list (gen-transform *identity-matrix-3x3*))
        (let* (;; p1, v1
               (shape1 (piece-points piece1))
               (p1+0 (modnth (+ point-from1 0)          shape1))
               (p1+d (modnth (+ point-from1 direction1) shape1))
               (v1   (vec3-sub-xy p1+d p1+0))
               ;; p2, v2
               (shape2 (piece-points piece2))
               (p2+0 (modnth (+ point-from2 0)          shape2))
               (p2+d (modnth (+ point-from2 direction2) shape2))
               (v2   (vec3-sub-xy p2+d p2+0))
               ;; ME (mirror and not)
               (mirror-and-empty (list t nil)))
          (mapcar #'(lambda (mirrorp)
                      (gen-transform (transform-matrix-{+p2.me.r.-p1} p1+0 v1 p2+0 v2 mirrorp)))
                  mirror-and-empty)))))

(defun make-transforms-by-point-and-edge-selection-piece-and-piece (select-piece1.piece2)
  " (cons an-transform-of-piece1 multiple-transforms-of-piece2) "
  (let* (;; :*1 => piece1, :*2 => piece2
         ;; piece1 does not move. Identity.
         (sel select-piece1.piece2)
         (transforms-piece1
           (make-transforms-by-point-and-edge-selection-parameters
            nil
            (assocdr :n1 sel) (assocdr :pm1 sel) (assocdr :p1 sel)
            (assocdr :n2 sel) (assocdr :pm2 sel) (assocdr :p2 sel)))
         ;; piece-moves
         (transforms-piece2
           (make-transforms-by-point-and-edge-selection-parameters
            t
            (assocdr :n2 sel) (assocdr :pm2 sel) (assocdr :p2 sel)
            (assocdr :n1 sel) (assocdr :pm1 sel) (assocdr :p1 sel))))
    (cons (car transforms-piece1) (identity transforms-piece2))))

(defun transform-shape-by-transformation-matrix
    (shape transformation-matrix
     &key
       (transform-coord-points  #'(lambda (f-transform coord-ps)  (mapcar f-transform coord-ps)))
       (transform-approx-points #'(lambda (f-transform approx-ps) (mapcar f-transform approx-ps))))
  (labels ((transform (v)
             (matrix3x3-vector3-product transformation-matrix v)))
    (shape 
     :pm-sign (shape-pm-sign shape)
     :coord-points  (funcall transform-coord-points  #'transform (shape-coord-points shape))
     :approx-points (funcall transform-approx-points #'transform (shape-approx-points shape)))))


(defun transform-shape-by-transformation-matrix-however-nil-approxs (shape transformation-matrix)
  (transform-shape-by-transformation-matrix
   shape transformation-matrix
   :transform-approx-points #'(lambda (f-transform coord-ps) f-transform coord-ps
                                      '()) ;; none of approx-points.
   ))

(defun all-contains-detection-piece-in-frame (frame-shape piece-shape)
  (let ((frame-coords (shape-coord-points frame-shape)))
    (and
     ;; edges of frame and piece are not collisioned.
     (not (shape-shape-boundary-collision-detection frame-shape piece-shape))
     ;; every point of piece are contained to frame
     (every #'(lambda (p) (point-inner-domain-p p frame-coords))
            (shape-approx-points piece-shape)))))

(defun none-contains-detection-piece-to-piece (piece-shape1 piece-shape2)
  (let ((piece1-coords (shape-coord-points piece-shape1))
        (piece2-coords (shape-coord-points piece-shape2)))
    (and
     ;; edges of piece1 and piece2 are not collisioned.
     (not (shape-shape-boundary-collision-detection piece-shape1 piece-shape2))
     ;; none point of piece_X are contained to piece_Y
     (notany #'(lambda (p1p) (point-inner-domain-p p1p piece2-coords))
             (shape-approx-points piece-shape1))
     (notany #'(lambda (p2p) (point-inner-domain-p p2p piece1-coords))
             (shape-approx-points piece-shape2)))))

(defun detect-proper-contain-for-synthesize-piece-and-piece (piece-shape1 piece-shape2)
  ;; shape's synthesizeable (points and edges) containing detection case by piece or frame.
  (let ((pm_1 (shape-pm-sign piece-shape1))
        (pm_2 (shape-pm-sign piece-shape2)))
    (cond (;; (+, +) || (-, -) ;; "piece to piece" or "hole to hole" where "frame" ⊂ "hole"
           (or (and (shape-plus-p  pm_1) (shape-plus-p  pm_2))
               (and (shape-minus-p pm_1) (shape-minus-p pm_2)))
           (none-contains-detection-piece-to-piece piece-shape1 piece-shape2))
          (;; (-, +) ;; "piece to frame"
           (and (shape-minus-p pm_1) (shape-plus-p  pm_2))
           (all-contains-detection-piece-in-frame  piece-shape1 piece-shape2))
          (;; (+, -) ;; flip and bring to "piece to frame"
           (and (shape-plus-p  pm_1) (shape-minus-p pm_2))
           ;; (warn "pm-pattern may wrong")
           (all-contains-detection-piece-in-frame piece-shape2 piece-shape1))
          (;; impossible piece.
           t
           (warn "piece has impossible sign")
           nil))))

(defun map-to-combination-selection-piece1.piece2
    (func select-piece1.piece2
     &key (shape-transformer #'transform-shape-by-transformation-matrix))
  ;; in some appears of,
  ;; (car . cdr) means (piece1_shape . piece2_maybely_shape_list)
  (let* ((sel select-piece1.piece2)
         ;; TransforMS, transform matrixes(Array)
         (tms (make-transforms-by-point-and-edge-selection-piece-and-piece sel))
         (tmas (mapcar #'transform-transformation-matrix tms))
         ;; ShapeS before transform
         (ss (mapcar #'piece-shape 
                     (cons (assocdr :p1 sel) (list (assocdr :p2 sel) (assocdr :p2 sel)))))
         ;; Spape'(Dush) eS. shapes after transforms.
         (sds (mapcar #'(lambda (shape transform)
                          (funcall shape-transformer shape transform))
                      ss tmas))
         ;; indexes
         (tm_f  (car tms))         (sd_f  (car sds))
         (tm_p1 (nth 0 (cdr tms))) (sd_p1 (nth 0 (cdr sds)))
         (tm_p2 (nth 1 (cdr tms))) (sd_p2 (nth 1 (cdr sds))))
    ;; map func to selected transform and shapes
    (mapcar #'(lambda (sd_px tm_px)
                (funcall func sd_f tm_f sd_px tm_px))
            (list sd_p1 sd_p2) (list tm_p1 tm_p2))))


(defun synthesize-piece-and-piece-by-selection-piece-or-fail (select-piece1.piece2)
  ;; make new (Common) piece if piece detection for collisioning is successed.
  ;; and filter disabled-transforms.
  (labels
      ((pm-sign-replaced-p (sd_1 sd_2)
         ;; (pm_1, pm_2) = (+, -) => flip it
         (if (and (shape-plus-p  (shape-pm-sign sd_1))
                  (shape-minus-p (shape-pm-sign sd_2)))
             t nil)))
    (remove
     nil
     (map-to-combination-selection-piece1.piece2
      #'(lambda (sd_f tm_f sd_px tm_px)
          (apply ;; flip if specific pm-sign pattern (describe this below)
           #'(lambda (sd_1 tm_1 sd_2 tm_2)
               (if (detect-proper-contain-for-synthesize-piece-and-piece sd_1 sd_2)
                   (synthesize-syntheable-piece-and-piece sd_1 tm_1 sd_2 tm_2)
                   nil))
           (if (pm-sign-replaced-p sd_f sd_px)
               (list sd_px tm_px sd_f  tm_f )
               (list sd_f  tm_f  sd_px tm_px)
               ;; if it is "frame to piece (+, -)" pattern (flipped pattern), 
               ;; it may cause wrong result at all-contains-detection,
               ;; flip its "frame to piece (+, -)" pattern into "piece to frame (-, +)" pattern.
               )))
      select-piece1.piece2))))

;;;; synthesize

(defparameter *id-counter* 1000)
(defun incf-id-counter! ()
  (incf *id-counter*))

(defun re-order-coordinate-points-by-transform (transform coordinate-points arc-dirp)
  ;; make coordinate-points list order into synthesize form:
  ;; rotate to beginning point (car),
  ;; on paired point at line-segment for synthesize (cadr),
  ;; reverse by direction (cddr).
  (let* ((rot-list (rot-left (transform-point-from transform) coordinate-points))
         (pm-sign (= -1 (transform-direction transform)))
         (pm-direction
           (if arc-dirp (not pm-sign) (identity pm-sign))))
    (cond ((eq t pm-direction)
           (cons (car rot-list) (identity (cdr rot-list))))
          (t 
           (cons (car rot-list) (reverse (cdr rot-list)))))))

(defun synthesize-syntheable-piece-and-piece (new-shape1 transform1 new-shape2 transform2)
  ;; synthesize OK transforms
  ;;
  ;; 1. get transform parameter and append edge points at coordinate.
  ;; 2. ommit points。
  ;; 3. fill domain by approx points.
  ;; 4. make-piece
  (let* (;; Coordinates[List]
         (c1 (shape-coord-points new-shape1))
         (c2 (shape-coord-points new-shape2))
         ;; rotated C, where δ1 = -(δ2)
         (rc1 (re-order-coordinate-points-by-transform transform1 c1 nil))
         (rc2 (re-order-coordinate-points-by-transform transform2 c2 t))
         ;; points Inserted RC
         (irc1 (insert-point-at-collisioning-edges rc2 rc1))
         (irc2 (insert-point-at-collisioning-edges rc1 rc2))
         ;; next coordintae-points is ommited append two IRCs.
         (synthed-coord-points
           (ommit-edge-points (append irc1 irc2)))
         ;; approxs points
         (approx-points
           ;; A-B (Set theory Subset) may be faster than 1ce below line
           (fill-shape-domain-by-approx-loading-points synthed-coord-points))
         ;; pm-sign
         (pm-sign 
           ((lambda (pm_1 pm_2)
              (cond ((and (shape-plus-p  pm_1) (shape-plus-p  pm_2)) *+shape*) ;; "piece and piece"
                    ((and (shape-minus-p pm_1) (shape-plus-p  pm_2)) *-shape*) ;; "piece to frame"
                    ((and (shape-minus-p pm_1) (shape-minus-p pm_2)) *-shape*) ;; "hole and hole"
                    ;; ((and (shape-plus-p  pm_1) (shape-minus-p pm_2)) *+shape*) ;; "hole to piece"
                    ;; where P1 - P2 is not implemented,
                    ((and (shape-plus-p  pm_1) (shape-minus-p pm_2)) *-shape*) ;; "frame to piece" into "piece to frame"
                    ))
            (shape-pm-sign new-shape1)
            (shape-pm-sign new-shape2)))
         ;; shape
         (synthed-new-shape
           (shape :pm-sign pm-sign
                  :coord-points synthed-coord-points
                  :approx-points approx-points)))

    (incf-id-counter!)
    ;;(format t "~A, ~A, (~A: ~A)~%"
    ;;  (length synthed-coord-points) (length approx-points)
    ;;  (length irc1) (length irc2))
    (piece :leaf-or-synthed :synthed
           :shape synthed-new-shape
           :id *id-counter*
           :function-sign *s-add*
           :transform1 transform1
           :transform2 transform2)))


;;; split line segment at the collisioning another shape's point coordinate.

(defun sew-for-each-point-aux (point coord-tuples-old coord-points-new)
  (cond ((null coord-tuples-old)
         (reverse  coord-points-new))
        (t
         (sew-for-each-point-aux
          point
          (if (point-on-line-segment-detection
               point (car (car coord-tuples-old)) (cdr (car coord-tuples-old)))
              (cons (cons point (cdr (car coord-tuples-old)))
                    (cdr coord-tuples-old))
              (cdr coord-tuples-old))
          (cons (car (car coord-tuples-old)) coord-points-new)))))


(defun insert-point-at-collisioning-edges (points shape-coord-points)
  ;; Unravel, stuff, sew. at the point where collisioning on another line segment.
  (cond ((null points) shape-coord-points)
        (t
         (insert-point-at-collisioning-edges
          (cdr points)
          (sew-for-each-point-aux
           (car points) (make-tuple-list shape-coord-points) '())))))


;;; ommit edge points

(defun ommit-edge-points-aux (3tuple-points)
  (let* ((pp0 (car  (car 3tuple-points)))  ;; point n plus 0
         (pp1 (cadr (car 3tuple-points)))  ;; point n plus 1
         (pp2 (cddr (car 3tuple-points)))) ;; point n plus 2
    ;;(format t "~A ~A ~A,  : ~A~%" pp0 pp1 pp2 nil)
    (cond ((null 3tuple-points) nil)
          ((vec3-ser= pp0 pp1) ;; (0,1)---(2) => (m1)... ;; Maybe 1. decide after check
           (identity (ommit-edge-points-aux (cdr 3tuple-points))))
          ((vec3-ser= pp0 pp2) ;; (0,2)---(1) => (2)--...
           (cons pp2 (ommit-edge-points-aux (cddr 3tuple-points))))
          (t                   ;; (1)-- => (1)--...
           (cons pp0 (ommit-edge-points-aux (cdr 3tuple-points)))))))


(defun ommit-edge-points (coord-points)
  ;; usage example
  ;; (ommit-edge-points '(#(0 0 1) #(0 5 0) #(5 5 0) #(4 0 0) #(5 5 0) #(5 0 0) #(0 0 1)))
  (cond
    ;; null
    ((null (cdr coord-points)) coord-points)
    ;; initial condition
    ;; (-1,1)--(0)--... => (2)--... ...--(-1)
    ((vec3-ser= (nth 1 coord-points) (car (last coord-points)))
     (ommit-edge-points (cddr coord-points)))
    ;; on modular condition
    (t
     (let ((ommit-once
             (ommit-edge-points-aux (make-3tuple-list coord-points))))
       ;; if is maybely not needed
       (if (equalp ommit-once coord-points)
           ommit-once
           (ommit-edge-points ommit-once))))))


#|
;;;; test
(insert-point-at-collisioning-edges
              '(#(2 5 0) #(0.1 0 0) #(0 4 0))
              '(#(0 0 1) #(5 0 0) #(5 5 0) #(0 5 0)))
|#
