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


;;; approx (loading) points

(defun inner-model-point (p_p p_c p_n rot-direction-sign)
  ;; Coord = Pc + (* (1/2 * CFL * 1/(1+ε)})
  ;;                 normalize({rot_direction * sign_pcn} * {dirVec(Pp - Pc) + dirVec(Pn - Pc)})
  ;; rot-direction-sign means clock wise or counter clockwise.
  (let ((sign_pcn ;; detect angle of pc is interior or exterior
          (if (< 0 (vec3-cross-xy (vec3-sub-xy p_p p_c) (vec3-sub-xy p_n p_c))) 1 -1)))
    (vec3-add-xy
     p_c
     (vec3-factor-xy (*  1/2  *default-approx-length* 1000/1001)
                     (vec3-normalize-xy
                      (vec3-factor-xy (* rot-direction-sign sign_pcn)
                                      (vec3-add-xy (vec3-normalize-xy (vec3-sub-xy p_p p_c))
                                                   (vec3-normalize-xy (vec3-sub-xy p_n p_c)))))))))

(defun fill-shape-domain-by-approx-loading-points (shape)
  (cond ((> 3 (length shape))
         nil)
        (t
         (let* ((rot-direction ;; detect clock wise or counter clock wise.
                  (if (point-inner-domain-p
                       (inner-model-point (nth 0 shape) (nth 1 shape) (nth 2 shape) 1) shape)
                      1 -1)))
           (mapcar #'(lambda (p_pcn) ;; point previous, current next
                       (let ((p_p (car  p_pcn)) (p_c (cadr p_pcn)) (p_n (cddr p_pcn)))
                         (inner-model-point p_p p_c p_n rot-direction)))
                   (make-3tuple-list shape))))))


#|
;; old implement
(defun fill-shape-domain-by-approx-loading-points (shape)
  (cond ((null shape) ;; 0 shape, zero-shape, it is zero element.
         nil)
        (t ;; otherwise, there is actual (physical) volume at the plane.
         (remove-if
          ;;#'(lambda (point) point nil ) ;;(not (point-inner-domnain-p point shape)))
          #'(lambda (point) (not (point-inner-domain-p point shape)))
          (approx-points-list (shape-domain-rect shape))
          ))))
|#

#|
;; unused implement
(defun fill-inner-line-by-points (p1 p2)
  "opened interval"
  (let* ((cfl-const (* 1/2 *default-approx-length*)) ;; 1/root(2) may be metter
         (l-line (vec3-length-xy (vec3-sub-xy p2 p1))) ;; length of line
         (n-points (ceiling (/ l-line cfl-const))) ;; n points to fill inner of line domain enoughly.
         ;;
         (delta-vector
           (vec3-factor-xy (/ l-line (+ n-points 1)) ;; length of delta
                           (vec3-normalize-xy (vec3-sub-xy p2 p1))))) ;; direction
    (loop for i from 1 to n-points
          collect (vec3-add-xy p1
                               (vec3-factor-xy i delta-vector)))))

(defun fill-shape-boundary-by-applox-loading-points (coords)
  (cond ((null coords) ;; zero-shape, it is zero element.
         '())
        (t ;; otherwise, there is actual (physical) boundary planes at the plane.
         (flatten (map-tuple #'fill-inner-line-by-points coords)))))

(defun fill-shape-domain-by-approx-loading-points (coords)
  (fill-shape-boundary-by-applox-loading-points coords))
|#



;;; piece piece collision detection

(defun collision-detection-piece-piece (p1 p2)
  ;; and boundary contain
  p1 p2 nil
  )

