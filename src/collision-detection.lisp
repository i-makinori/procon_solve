(in-package :puzzle-1617)

;;;; collision detection

;;;; collision detection

;;; point on line-segment

(defun point-on-line-segment-detection (point point1 point2)
  (let ((v1 (vec3-sub-xy point point1))
        (v2 (vec3-sub-xy point point2)))
    (and (not (vec3-ser= point point1)) ;; not on point1
         (not (vec3-ser= point point2)) ;; not on point2
         (vec3-ser= (identity (vec3-normalize-xy v1)) ;; point are inner range of edge P1--P2.
                    (vec3-inverse-xy (vec3-normalize-xy v2))))))
    


;;; collision detection for boundary lines

(defun point-line-collision-detection (point-a1 point-a2 point-b1 point-b2)
  ;; {   0 < (A1A2 X A1B1) * (A1A2 X A1B2)
  ;;  && 0 < (B1B2 X B1A1) * (B1B2 X B1A2)}
  ;; => 2D line A1A2 and B1B2 are crossing.
  ;; Ref: https://qiita.com/zu_rin/items/e04fdec4e3dec6072104
  (let ((vec-a1a2 (vec3-sub-xy point-a2 point-a1))
        (vec-b1b2 (vec3-sub-xy point-b2 point-b1)))
    (and
     (> (- -0.0001 *standard-error*) ;; round tolerance, remove line-on points.
        (* (vec3-cross-xy vec-a1a2 (vec3-sub-xy point-b1 point-a1))   ;; A1A2 X A1B1
           (vec3-cross-xy vec-a1a2 (vec3-sub-xy point-b2 point-a1)))) ;; A1A2 X A1B2
     (> (- -0.0001 *standard-error*) ;; round tolerance, remove line-on points.
        (* (vec3-cross-xy vec-b1b2 (vec3-sub-xy point-a1 point-b1))   ;; B1B2 X B1A1
           (vec3-cross-xy vec-b1b2 (vec3-sub-xy point-a2 point-b1)))) ;; B1B2 X B1A2
     )))

(defun shape-shape-boundary-collision-detection (shape1 shape2)
  (labels ((some-line-collisions-p (papb pnpms)
             (cond ((null pnpms) nil)
                   ((point-line-collision-detection (car papb) (cdr papb) (caar pnpms) (cdar pnpms))
                    t)
                   (t (some-line-collisions-p papb (cdr pnpms))))))
    (let* ((tu-points1 (make-tuple-list (shape-coord-points shape1)))
           (tu-points2 (make-tuple-list (shape-coord-points shape2))))
      ;; if some edge by 2 points crosses to another edges, piece1 and piece2 are touch crossing.
      (or (some #'(lambda (p1ap1b) (some-line-collisions-p p1ap1b tu-points2)) tu-points1)
          (some #'(lambda (p2ap2b) (some-line-collisions-p p2ap2b tu-points1)) tu-points2)))))



;;;; contains detection for aprrox point.

(defun angle (p_center p1 p2)
  "p_* :: vector of coordinate"
  ;; Ref: https://tjkendev.github.io/procon-library/python/geometry/point_inside_polygon.html
  (let* ((v1 (vec3-sub-xy p1 p_center))
         (v2 (vec3-sub-xy p2 p_center))
         (dvv (vec3-dot-xy   v1 v2))
         (cvv (vec3-cross-xy  v1 v2)))
    (atan cvv dvv)))

(defun point-inner-domain-p (point shape)
  "shape is vec3 list of piece."
  ;; ?? does not contains above line points. because direction > 0 . ??
  ;; Ref: https://www.nttpc.co.jp/technology/number_algorithm.html
  (let* ((angles (map-tuple #'(lambda (sp1 sp2) (angle point sp1 sp2))
                            shape)))
    (>= (abs (apply #'+ angles)) (- *pi*2* *standard-error*))))

(defun shape-domain-rect (shape)
  "(list x-min y-min x-max y-max)"
  (let ((x-coords (mapcar #'vec3-x shape))
        (y-coords (mapcar #'vec3-y shape)))
    (mapcar #'(lambda (fs) (apply (car fs) (cdr fs)))
            (list (cons #'min x-coords) (cons #'min y-coords)
                  (cons #'max x-coords) (cons #'max y-coords)))))

(defun domain-rect-x-min (domain-rect) (nth 0 domain-rect))
(defun domain-rect-y-min (domain-rect) (nth 1 domain-rect))
(defun domain-rect-x-max (domain-rect) (nth 2 domain-rect))
(defun domain-rect-y-max (domain-rect) (nth 3 domain-rect))

(defparameter *default-approx-length* (* 1 1)) ;; 1/2, 1/3, 1/4, 1/8 ...

(defun approx-points-list (domain-rect &optional (approx-length *default-approx-length*))
  "domain :: (list x_min y_min x_max y_max)"
  (let ((y-from (- (domain-rect-y-min domain-rect) (/ approx-length 2)))
        (y-to   (+ (domain-rect-y-max domain-rect) (/ approx-length 2)))
        (x-from (- (domain-rect-x-min domain-rect) (/ approx-length 2)))
        (x-to   (+ (domain-rect-x-max domain-rect) (/ approx-length 2))))
    (flatten 
     (loop for y from y-from to y-to by approx-length
           collect (loop for x from x-from to x-to by approx-length
                         collect (vec3 x y 1))))))


(defun fill-shape-domain-by-approx-loading-points (shape)
  (cond ((null shape) ;; 0 shape, zero-shape, it is zero element.
         nil)
        (t ;; otherwise, there is actual (physical) volume at the plane.
         (remove-if
          ;;#'(lambda (point) point nil ) ;;(not (point-inner-domnain-p point shape)))
          #'(lambda (point) (not (point-inner-domain-p point shape)))
          (approx-points-list (shape-domain-rect shape))
          ))))

;;; piece piece collision detection

(defun collision-detection-piece-piece (p1 p2)
  ;; and boundary contain
  p1 p2 nil
  )



;;; detect congruent of piece and piece

(defun detect-piece-point-selection-makes-congruent-transform (select-piece.piece)
  (let ((detect_1_2
          (map-to-combination-selection-frame.piece
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
          (whole-set-of-point-and-edge-selections-pieces-to-frame
           piece1 (list piece2))))

(defun detect-piece-congruent (piece1 piece2)
  ;; detect piece1 === piece2
  (and
   ;; == pm_sign
   (eq (piece-pm-sign piece1)
       (piece-pm-sign piece2))
   ;; == num edge points
   (= (length (piece-coord-points piece1)) 
      (length (piece-coord-points piece2)))
   ;; == primary piecese which composes its piece.
   (set-equal (mapcar #'piece-id (list-of-primary-piece-list-of-synthesized-piece piece1))
              (mapcar #'piece-id (list-of-primary-piece-list-of-synthesized-piece piece2)))
   ;; or 
   (or
    ;; root is zero shape
    (and (zero-shape-piece-p piece1)
         (zero-shape-piece-p piece2))
    ;; exist of congruent transform
    (detect-piece-exist-congruent-transform piece1
                                            piece2))))

#|

> (setq *search1*
              (sort-by-delta_points
               (all-synthesizeable-patterns-of-pieces-to-frame
                (car *example-problem-9*) (cdr *example-problem-9*))))
nil
> (detect-piece-congruent (nth 1 *search1*) (nth 0 *search1*))
|#
