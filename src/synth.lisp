
(in-package :procon)


;;;; synth-piece ;;;;;;;;;;;;;;;;

(defparameter *plus* +1 "synth direction to +")
(defparameter *minus* -1 "synth direction to -")

(defstruct (synth (:conc-name synth-))
  piece direction synth-from)

(defun synth (piece direction synth-from-order-of-spot)
  "make-synth"
  (make-synth 
   :piece piece
   :direction direction
   :synth-from synth-from-order-of-spot))


;;;; struct 

(defstruct (easy-piece)
  spots degrees is-frame)

(defun easy-piece (spots degrees is-frame)
  "structure make it more useful to synth piece.
easy-piece :: [spot], [degree], is-frame

which can intepret special synth"
  (make-easy-piece :spots spots
                   :degrees degrees
                   :is-frame is-frame))

(defun easy-piece->piece (easy-piece)
  (piece (easy-piece-spots easy-piece)
         (easy-piece-degrees easy-piece)
         (easy-piece-is-frame easy-piece)
         nil nil))
         
(defun piece-from--easy-piece+synth+synth (easy-piece synth-from synth-to)
  (piece (easy-piece-spots easy-piece)
         (easy-piece-degrees easy-piece)
         (easy-piece-is-frame easy-piece)
         synth-from synth-to))


;;;; function 
(defun synth->maybe-piece (synth-from synth-to)
  (let* 
      ((easy-piece-from (synth->easy-piece synth-from))
       (easy-piece-to (synth->easy-piece synth-to))
       
       )
    #|
    (piece
     '() '() '()
     synth1 synth2))
    |#
    ))


#|
(defun rotate-piece (piece angle)
  (make-piece
   :vectors (mapcar #'(lambda (v) 
                        (vec (- (* (vx v) (cos angle)) (* (vy v) (sin angle)))
                             (+ (* (vx v) (sin angle)) (* (vy v) (cos angle )))))
                    (piece-vectors piece))
   :degrees (piece-degrees piece)))

(defun turn-orver-piece (piece two-side)
  "turn orver in y-axis 
nil:turnout, t:surfece"
  (if two-side piece
      (make-piece 
       :vectors (mapcar #'(lambda (v) (vec (- (vx v)) (vy v))) (piece-vectors piece))
       :degrees (piece-degrees piece)
       )))
|#

(defun maybe-easy-piece-by-rotate (degree easy-piece)
  )

(defun maybe-turn-over-piece (easy-piece)
  )

(defun easy-piece-include-detectuon (easy-piece-from easy-piece-to)
  )

(defun easy-piece-collision-detection (easy-piece1 easy-piece2)
  )

(defun synth->easy-piece (synth)
  "structure/function make it more useful synth to synth piece"
  (let* ((piece (synth-piece synth))
         (direction (synth-direction synth))
         (rotate-num (synth-synth-from synth))
         ;;
         (if-reverse (if (equal direction *plus*) #'id #'reverse))
         (rotated-spots (rotate-list (piece-spots piece) rotate-num))
         (rotated-degrees (rotate-list  (piece-degrees piece) rotate-num))
         (origin-spot (nth (synth-synth-from synth) (piece-spots piece))))
    (easy-piece 
     (mapcar #'(lambda (spot)
                 (spot (- (spot-x spot) (spot-x origin-spot))
                       (- (spot-y spot) (spot-y origin-spot))))
             (cons (car rotated-spots)
                   (funcall if-reverse (cdr rotated-spots))))
     (cons (car rotated-degrees)
           (funcall if-reverse (cdr rotated-degrees)))
     (piece-is-frame piece))))



;;;; test

;; ref : *test-piece-for-synth1* *test-piece-for-synth2*
(defparameter *test-synth1* 
  (synth *test-piece-for-synth1* *plus* 5))

(defparameter *test-synth2*
  (synth *test-piece-for-synth2* *minus* 4))

(defparameter *test-synthed-piece*
  (synth->maybe-piece *test-synth1* *test-synth2*))
