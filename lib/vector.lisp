(in-package #:procon)


;;;; struct

(defmacro vec (dx dy)
  `(cons ,dx ,dy))

(defmacro deg (deg)
  `,deg)

(defmacro dimension (dimension)
  "length, how long is it"
  `,dimension)


;; functions

(defun dist (vec1 vec2)
  (sqrt (+ (expt (car (dxdy vec1 vec2)) 2)
           (expt (cdr (dxdy vec1 vec2)) 2))))

(defun vector-dist (vec)
  (sqrt (+ (expt (vx vec) 2)
           (expt (vy vec) 2))))

(defun vx (vec)
  (car vec))

(defun vy (vec)
  (cdr vec))

(defun dxdy (vec1 vec2)
  (cons (- (vx vec2) (vx vec1))
        (- (vy vec2) (vy vec1))))

(defun vector-to-point (vector)
  (list (car vector) (cdr vector)))

(defun vector-to-line (vector-start vector-delta)
  (line (vx vector-start) (vy vector-start)
        (vx vector-delta) (vy vector-delta)))


(defun line-collision-detection (line1 line2)
  "line hit-judge"
  
  )























