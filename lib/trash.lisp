
;;;; there are some functions that is not called by any functions


#|

;;; vector.lisp

(defun vec-coord-include-detection (p1-v1 p1-v2 p2-v1 p2-v2 )
  "p:piece, v:vector, 1:first, 2:last, dir:direction"
  (let ((p1-v1-deg (vectors-to-angle *angle-vec-criteria* *vec-origin* p1-v1))
        (p1-v2-deg (vectors-to-angle *angle-vec-criteria* *vec-origin* p1-v2))
        (p2-v1-deg (vectors-to-angle *angle-vec-criteria* *vec-origin* p2-v1))
        (p2-v2-deg (vectors-to-angle *angle-vec-criteria* *vec-origin* p2-v2)))
    (or (in-range? p1-v1-deg p1-v2-deg p2-v1-deg)
        (in-range? p1-v1-deg p1-v2-deg p2-v2-deg)
        (in-range? p2-v1-deg p2-v2-deg p1-v1-deg)
        (in-range? p2-v1-deg p2-v2-deg p1-v2-deg))))

(defun in-range? (val1 val2 val-judge)
  (if (< val1 val2)
      (and (< val1 val-judge) (< val-judge val2))
      (and (> val1 val-judge) (> val-judge val2))))


|#


#|

;;; piece.lisp


|#
