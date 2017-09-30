
(in-package :procon)

;;;; synth-piece ;;;;;;;;;;;;;;;;

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

;;;; params 

(defparameter plus +1)
(defparameter minus -1)


;;;; function 
(defun synth->maybe-piece (synth-from synth-to)
  (let* 
      ((piece1 (synth-piece synth-from))
       (piece1-spots (piece-spots piece1))
       (piece1-degs (piece-degrees piece1))
       
       (piece2 (synth-piece synth-to))
       (piece2-spots (piece-spots piece2))
       (piece2-degs (piece-degrees piece2))
       
       (angle2 (vector-angle (spot->vec (rotate-nth (synth-synth-from synth-to)
                                                    piece2-spots))
       ))
       
       
       )

    angle2
    #|
    (piece
     '() '() '()
     synth1 synth2))
    |#

    ))

(defun synth->easy-piece (synth)
  "synth -> synth
function make it more useful synth to synth piece"
  (let* ((piece (synth-piece synth))
         (direction (synth-direction synth))
         (rotate-num (synth-synth-from synth))
         ;;
         (if-reverse (if (equal direction plus) #'id #'reverse))
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
  (synth *test-piece-for-synth1* plus 5))

(defparameter *test-synth2*
  (synth *test-piece-for-synth2* minus 4))

(defparameter *test-synthed-piece*
  (synth->maybe-piece *test-synth1* *test-synth2*))
