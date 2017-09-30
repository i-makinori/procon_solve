
(in-package :procon)

;;;; data-structure

;; Piece ::= {spots::[Spot], synth-from::Synth||Nil, synth-to::Synth||Nil}
;; Synth ::= {Piece, direction::((+1||-1)::=::Bool), synth-from::Rotate-Ord}
;; Spot ::= x::Int, y::Int

;; in hashtable, 
;; element =:: table=::(Id::S), Piece

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

(defun spots->piece (spots)
  (let ((degrees (degree-adjust
                  (map-tuple/c #'clock-wise-angle 3
                               (rotate-list (spots->vecs spots) -1)))))
    (piece
     spots degrees nil nil)))


;;;; util

(defun piece-able-states (piece)
  ;; list of piece of conversion possible.
  ;; which is set of discrete spots, flip overd of it, and so on.
  ;;
  ;; (rotate(spots) => [spots]) 
  ;; .[poins] => all spots are included in discrete spot.
  )


(defun degree-adjust (degrees)
  "in n-polygon. when degree-rength is not (n-2)*pi, let degree-length (n-2)*pi "
  (cond ((error-round-deg= pi
                           (/ (reduce #'+ degrees) (- (length degrees) 2)))
         degrees)
        (t (mapcar #'(lambda (deg) (- 2pi deg)) degrees))))



;;;; some function

(defun piece-height (piece)
  (let ((y-val-list 
         (mapcar #'(lambda (spot) (spot-y spot))
                 (piece-spots piece))))
    
    (- (apply #'max y-val-list)
       (apply #'min y-val-list))))

(defun piece-width (piece)
  (let ((x-val-list 
         (mapcar #'(lambda (spot) (spot-x spot))
                 (piece-spots piece))))
    
    (- (apply #'max x-val-list)
       (apply #'min x-val-list))))
