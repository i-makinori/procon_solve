
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
    ;;(piece-id piece1) point-from1 direction1 
    ;;(piece-id piece2) point-from2 direction2)
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
    ;; and filter disable-transforms.
    (remove
     nil
     (mapcar #'(lambda (shape tm)
                 (cond ((all-contains-detection-piece-in-frame sd_f shape)
                        (incf-id-counter!)
                        (piece :leaf-or-synthed :synthed
                               :shape shape ;; 
                               :id *id-counter*
                               :function-sign '+
                               :transform1 tm_f ;; frame's transform
                               :transform2 tm)) ;; piece-to's transform
                       (t nil)))
             (list sd_p1 sd_p2) (list tm_p1 tm_p2)))))



(defun all-synthesizeable-patterns-of-pieces-to-frame (frame piece-list)
  (flatten
   (remove nil 
           (mapcar #'synthesize-piece-to-frame-by-selection-piece-or-fail
                   (whole-set-of-point-and-edge-selections-pieces-to-frame
                    frame piece-list)))))

#|
;; test
(mapcar #'piece-coord-points (synthesize-piece-to-frame-by-selection-piece-or-fail
                              (nth 0 (whole-set-of-point-and-edge-selections-pieces-to-frame
                                         (car *example-problem-10*)
                                         (cdr *example-problem-10*)))))

(write-piece-list-as-html
              (all-synthesizeable-patterns-of-pieces-to-frame
               (car *example-problem-9*) (cdr *example-problem-9*)))

|#


    

;;; transform

#|
(defun transform-matrix-of-transform (transform)
  ;; (Mirror * Rotate * ParallelMove) * points
  ;; Parallel Move to origin point, Rotate to x axis, Mirror by x axis.
  (reduce
   #'matrix3x3-product
   (list (transform-parallel-move 2 3)
         (transform-rotate (* 1/4 *pi*2*))
         (transform-mirror-axis-x-axis -1)))
  '(+1  -1))

(defun apply-transform (transform)
  (let* ((piece-ago (transform-piece transform))
         (shape-ago (piece-shape piece-ago)))
    (piece-shape
     :pm-sign (shape-pm-sign shape-ago)
     :coord-points
     :approx-points nil
     
     )))


(defun try-synthesize-piece (transform1 transform2)
  (let* (;; trdpc:: transformed piece
         (trdpe-1 nil)
         (trdpe-2 nil))
    (piece
     :leaf-or-synthed 'synthed
     :shape ()

     
     )))

(defun synth (p1 p2)
  nil)


;;(defun synthesize ()
;;)

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

