(in-package #:procon)

(defstruct piece
  (vectors '())
  (degrees '())
  (dists '())
  )


;;; util

(defun vectors-to-angle (vec1 vec2 vec3)
  (angle (vector-to-point vec1)
         (vector-to-point vec2)
         (vector-to-point vec3)))


(defun coord-to-piece (coordinates)
  (let* ((vectors (map-tuple/c #'dxdy 2 coordinates))
         (degrees (map-tuple/c #'vectors-to-angle 3 coordinates))
         (dists (mapcar #'vector-dist vectors)))
    (make-piece
     :vectors vectors
     :degrees degrees
     :dists dists)))


;;;; test

(defparameter *test-piece*
  (coerce (make-array 3 :element-type 'cons
                      :initial-contents #((10 . 10) (10 . 40) (40 . 10)))
          'list))

(defparameter *test-piece2*
  '((30 . 10) (10 . 40) (30 . 20) (50 . 40)))


(defun test-make-piece (piece)
  
  )
