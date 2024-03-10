(in-package :puzzle-1617)

;;;; partial problem
;;; such as line or angle




;; 1. all of length of frame segment has equall length by the synthesize of other pieces.
;;    ∀(x±1) . (x±1) is frame_segment
;;    => ∃N . Length((x±1)) = Σ Length(piece_segment_(ni±1)) while N={n1,n2,...,nn}
;; 2. all angle of all points has 360-degree synthesize with other pieces.
;;    ∀x. x is piece_point
;;    => ∃N. Σ Angle(piece_angle_ni) while N={n1,n2,...,nn} = 2*π


;; 部分問題を先に解く
;; 部分問題を解けない頂点や辺を起点とする合成は行わないことで、計算を削減する。


(defun solve-partial-problem-aux (value-accessor-list memo-structure)
  nil
  )

(defparameter *memo-of-partial-problem*
  nil)

(defparameter *depth-const-of-partial-problem*
  3)

(defun solve-partial-problem (value-accessor-list )
  nil
  )
