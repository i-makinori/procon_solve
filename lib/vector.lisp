(in-package #:procon)


;;;; struct

(defmacro vec (dx dy)
  `(cons ,dx ,dy))

(defmacro deg (deg)
  `,deg)

(defmacro dimension (dimension)
  #| length, how long|#
  `,dimension)


;; functions

(defun dist (vec1 vec2)
  (sqrt (+ (expt (car (dxdy vec1 vec2)) 2)
           (expt (cdr (dxdy vec1 vec2)) 2))))

(defun vx (vec)
  (car vec))

(defun vy (vec)
  (cdr vec))

(defun dxdy (vec1 vec2)
  (cons (- (vx vec2) (vx vec1))
        (- (vy vec2) (vy vec1))))


;;; util

(defun coord-to-piece (coordinates)
  ()
  )
