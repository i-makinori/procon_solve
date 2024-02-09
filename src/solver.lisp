
(in-package :puzzle-1617)

;;;; piece

(defun point (x y)
  (vec3 x y 1))

;;;; nomial

;;;; polynomial

;;;; vector, matrix

;;;; transforms



;;;; polynomial

;;; sample shape
(defparameter *shape1* (list #(0 18 1) #(6 10 1) #(13 14 1) #(14 18 1)))

;;;; collision detection

(defun collision-detection-piece-piece (p1 p2)
  p1 p2 nil
  )

(defun point-inner-domnain-p (point shape)
  "shape is vec3 list of piece."
  ;; does not contains above line points. because direction > 0 .
  ;; ref: http://www.sousakuba.com/Programming/gs_hittest_point_triangle.html
  (let* ((edge-vecs (map-tuple #'(lambda (coord1 coord2) (vec3-sub-xy coord2 coord1)) shape))
         (to-point-vecs (mapcar #'(lambda (coord) (vec3-sub-xy point coord)) shape))
         (crosses-xy (mapcar #'vec3-cross-xy edge-vecs to-point-vecs)))
    (or (every #'(lambda (dir) (> dir 0)) crosses-xy)
        (every #'(lambda (dir) (< dir 0)) crosses-xy))))

;;;; synth

(defun synth (p1 p2)
  nil)




;;;; search solution



