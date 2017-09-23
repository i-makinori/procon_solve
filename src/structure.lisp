(in-package :procon)

;;;; data-structure

;; Piece ::= {points::[Point], synth-from::Synth||Nil, synth-to::Synth||Nil}
;; Synth ::= {Piece, direction::((+1||-1)::=::Bool), synth-from::Rotate-Ord}
;; Point ::= x::Int, y::Int

;; in hashtable, 
;; element =:: table=::(Id::S), Piece


(defun point (dx dy)
  `((:x ,dx) (:y ,dy)))

(defun points (point-list)
  `(:points ,point-list))

(defun synth (piece direction synth-from-order-of-point)
  `((:piece ,piece)
    (:direction ,direction)
    (:synth-from ,synth-from-order-of-point)))

(defun piece (points synth-from synth-to)
  `((:points ,points)
    (:synth-from ,synth-from)
    (:synth-to ,synth-to)))


;;;; util
(defun piece-able-states (piece)
  ;; list of piece of conversion possible.
  ;; which is set of discrete points, flip overd of it, and so on.
  ;;
  ;; (rotate(points) => [points]) 
  ;; .[poins] => all points are included in discrete point.
  )



;;;; test
;; sample json of synth and piece
;; https://github.com/nnct-jo-ken/procon2017_kyogi_server/wiki/piece-synth-in-JSON

