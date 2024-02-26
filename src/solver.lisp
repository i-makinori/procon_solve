
(in-package :puzzle-1617)

;;;; piece

;;;; nomial

;;;; polynomial

;;;; vector, matrix

;;;; transforms

;;;; polynomial


;;;; 


;;;; synth

(defun whole-set-of-synthesize (frame-piece piece-list)
  (let ((transform-frame (transform :piece frame-piece)) ;; identity transform
        )
    transform-frame
    
    ))

(defun synth-frame-to-piece (frame frame-point frame-direction
                             piece piece-point piece-dorection)
  )


;;; group set of points selections for synthesize

#| cutten duplicated selections
;; consider fill frame by pieces, selection patten is,
;; frame piece2 piece3 piece4 ...
.  0 1 1 1 ... ;; list of synthable pattern of piece to frame
.  0 0 0 0 ... ;; piece and piece is omitted
.  0 0 0 0 ... ;;
.  0 0 0 0 ... ;;
...
...
|#

(defun from-m-to-n-list (from-m to-n)
  (loop for i from from-m to to-n
        collect i))

(defun whole-set-of-point-and-edge-selections-piece-piece (piece1 piece2)
  ;; 
  (apply #'append
         (loop
           for i from 0 to (1- (length (piece-points piece1)))
           collect (loop 
                     for j from 0 to (1- (length (piece-points piece2)))
                     collect (mapcar #'(lambda (sign)
                                         `((:p1 . ,piece1) (:n1 . ,i) (:pm1 . ,(car sign))
                                           (:p2 . ,piece2) (:n2 . ,j) (:pm2 . ,(cdr sign))))
                                     '((+1 . +1) (+1 . -1) (-1 . +1) (-1 . -1)))))))

(defun flat-2d-nest-list (list-list-list)
  (apply #'append (apply #'append  list-list-list)))

(defun whole-set-of-point-and-edge-selections-pieces-to-frame (frame-piece piece-list)
  (flat-2d-nest-list
   (mapcar #'(lambda (p) (whole-set-of-point-and-edge-selections-piece-piece frame-piece p))
           piece-list)))

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
             (transform :function-sign '+
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

(defun make-transforms-by-point-and-edge-selection-piece-to-frame (select-frame.piece)
  " (cons an-transform-of-frame multiple-transforms-of-piece) "
  (let* (;; :*1 => frame, :*2 => piece
         ;; frame does not move. Identity.
         (sel select-frame.piece)
         (transforms-frame
           (make-transforms-by-point-and-edge-selection-parameters
            nil
            (assocdr :n1 sel) (assocdr :pm1 sel) (assocdr :p1 sel)
            (assocdr :n2 sel) (assocdr :pm2 sel) (assocdr :p2 sel)))
         ;; piece-moves
         (transforms-piece
           (make-transforms-by-point-and-edge-selection-parameters
            t
            (assocdr :n2 sel) (assocdr :pm2 sel) (assocdr :p2 sel)
            (assocdr :n1 sel) (assocdr :pm1 sel) (assocdr :p1 sel))))
    (cons (car transforms-frame) (identity transforms-piece))))

(defun transform-shape-by-transformation-matrix (shape transformation-matrix)
  (labels ((transform (v)
             (matrix3x3-vector3-product transformation-matrix v)))
    (shape :pm-sign (shape-pm-sign shape)
           :coord-points (mapcar #'transform (shape-coord-points shape))
           :approx-points (mapcar #'transform (shape-approx-points shape)))))

(defparameter *id-counter* 1000)
(defun incf-id-counter! ()
  (incf *id-counter*))

(defun all-contains-detection-piece-in-frame (frame-shape piece-shape)
  (let (; C1: every point of piece are contained to frame
        (c1 (every #'(lambda (p) (point-inner-domain-p p 
                                                       (shape-coord-points frame-shape)))
                   (shape-approx-points piece-shape)))
        ;; C2: edges of frame and piece are not collisioned.
        (c2 (not (shape-shape-boundary-collision-detection frame-shape piece-shape))))
    ;; C1 && C2
    (and c1 c2)))

(defun synthesize-piece-to-frame-by-selection-piece-or-fail (select-frame.piece)
  ;; in some appears of,
  ;; (car . cdr) means (frame_shape . piece_maybely_shape_list)
  (let* ((sel select-frame.piece)
         ;; transforms, transform matrixes(Array)
         (tms (make-transforms-by-point-and-edge-selection-piece-to-frame sel))
         (tmas (mapcar #'transform-transformation-matrix tms))
         ;; Spape'(Dush) eS. shapes after transforms.
         (ss (mapcar #'piece-shape 
                     (cons (assocdr :p1 sel) (list (assocdr :p2 sel) (assocdr :p2 sel)))))
         (sds (mapcar #'(lambda (shape transform)
                           (transform-shape-by-transformation-matrix shape transform))
                      ss tmas))
         ;; indexes
         (tm_f  (car tms))         (sd_f  (car sds))
         (tm_p1 (nth 0 (cdr tms))) (sd_p1 (nth 0 (cdr sds)))
         (tm_p2 (nth 1 (cdr tms))) (sd_p2 (nth 1 (cdr sds))))
    ;; make new (frame-)piece if piece is contained to frame
    ;; and filter disabled-transforms.
    (remove
     nil
     (mapcar #'(lambda (sd_px tm_px)
                 (if (all-contains-detection-piece-in-frame sd_f sd_px)
                     ;; able to put piece into frame
                     (synthesize-syntheable-piece-to-frame  sd_f tm_f sd_px tm_px)
                     ;; diable to put piece into frame
                     nil))
             (list sd_p1 sd_p2) (list tm_p1 tm_p2)))))

(defun all-synthesizeable-patterns-of-pieces-to-frame (frame piece-list)
  (flatten
   (remove nil 
           (mapcar #'synthesize-piece-to-frame-by-selection-piece-or-fail
                   (whole-set-of-point-and-edge-selections-pieces-to-frame
                    frame piece-list)))))

;;;; synthesize

(defun synthesize-syntheable-piece-to-frame (new-shape1 transform1 new-shape2 transform2)
  ;; synthesize OK transforms
  ;;
  ;; 1. get transform parameter and append edge points at coordinate.
  ;; 2. ommit points。
  ;; 3. fill domain by approx points.
  ;; 4. make-piece
  (let* (;; Coordinates
         (c1 (shape-coord-points new-shape1))
         (c2 (shape-coord-points new-shape2))
         ;; rotated C
         (rc1 (re-order-coordinate-points-by-transform transform1 c1 nil))
         (rc2 (re-order-coordinate-points-by-transform transform2 c2 t))
         ;; insert pointed RC
         (irc1 (insert-point-at-collisioning-edges rc2 rc1))
         (irc2 (insert-point-at-collisioning-edges rc1 rc2))
         ;; next coordintae-points is ommited append two IRCs.
         (synthed-coord-points
           (ommit-edge-points (append irc1 irc2)))
         ;; approxs points
         (approx-points
           ;; A-B (Set theory Subset) may be faster than 1ce below line
           (fill-shape-domain-by-approx-loading-points synthed-coord-points))
         ;; shape
         (synthed-new-shape
           (shape :pm-sign '-
                  :coord-points synthed-coord-points
                  :approx-points approx-points)))

    (incf-id-counter!)
    ;;(format t "HOGE ~A, ~A~%" (transform-point-from transform1) (transform-point-from transform2))
    ;;(format t "  ~A~%  ~A ~%" rc1 rc2)
    (piece :leaf-or-synthed :synthed
           :shape synthed-new-shape
           :id *id-counter*
           :function-sign '+
           :transform1 transform1
           :transform2 transform2)))


;;;; util to point omitting

(defun rot-left (n l)
  (append (nthcdr n l) (butlast l (- (length l) n))))

(defun rot-right (n l)
  (rot-left (- (length l) n) l))

(defun re-order-coordinate-points-by-transform (transform coordinate-points arc-dirp)
  (let* ((rot-list (rot-left (transform-point-from transform) coordinate-points))
         (pm-sign (= -1 (transform-direction transform)))
         (pm-direction
           (if arc-dirp (not pm-sign) (identity pm-sign))))
    (cond ((eq t pm-direction)
           (cons (car rot-list) (identity (cdr rot-list))))
          (t 
           (cons (car rot-list) (reverse (cdr rot-list)))))))


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

;;;

#|
;; test
(mapcar #'piece-coord-points (synthesize-piece-to-frame-by-selection-piece-or-fail
                              (nth 0 (whole-set-of-point-and-edge-selections-pieces-to-frame
                                         (car *example-problem-10*)
                                         (cdr *example-problem-10*)))))

(write-piece-list-as-html
              (all-synthesizeable-patterns-of-pieces-to-frame
               (car *example-problem-9*) (cdr *example-problem-9*)))


(insert-point-at-collisioning-edges
              '(#(2 5 0) #(0.1 0 0) #(0 4 0))
              '(#(0 0 1) #(5 0 0) #(5 5 0) #(0 5 0)))

|#


;;;; search solution

;; minus to frame method

(defun state (current-frame remains-pieces points-length)
  `((:current-frame . ,current-frame)
    (:remains . ,remains-pieces)
    (:points-length . ,points-length)))

(defun state-remains (state)
  (cdr (assoc :remains state))
  )

(defun search-solution-aux (state-stack)
  (let* ((current-state (car state-stack))
         (stack (cdr state-stack))) ;; older sorted stack
    stack
    (cond ((null (state-remains current-state)) ;; remain-piece is nil
           current-state) ;; it is goal
          )))

(defun search-solution (frame pieces)
  
  )

