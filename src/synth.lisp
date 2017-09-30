
(in-package :procon)

;;;; synth-piece ;;;;;;;;;;;;;;;;

;; Piece ::= {spots::[Spot], synth-from::Synth||Nil, synth-to::Synth||Nil}
;; Synth ::= {Piece, direction::((+1||-1)::=::Bool), synth-from::Rotate-Ord}
;; Spot ::= x::Int, y::Int

;; in hashtable, 
;; element =:: table=::(Id::S), Piece

#|
(defstruct (synth (:conc-name synth-))
  piece direction synth-from)

(defun synth (piece direction synth-from-order-of-spot)
  "make-synth"
  (make-synth 
   :piece piece
   :direction direction
   :synth-from synth-from-order-of-spot))

(defstruct (piece (:conc-name piece-))
  spots degrees synth-from synth-to)

(defun piece (spots degrees synth-from synth-to)
  "make-piece"
  (make-piece :spots spots
              :degrees degrees
              :synth-from synth-from
              :synth-to synth-to))
|#
