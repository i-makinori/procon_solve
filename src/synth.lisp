
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

(defstruct (easy-piece (:conc-name epiece-))
  spots degrees is-frame)

(defun easy-piece (spots degrees is-frame)
  "structure make it more useful to synth piece.
easy-piece :: [spot], [degree], is-frame

which can intepret special synth"
  (make-easy-piece :spots spots
                   :degrees degrees
                   :is-frame is-frame))

;; transform

(defun easy-piece->piece (easy-piece)
  (piece (epiece-spots easy-piece)
         (epiece-degrees easy-piece)
         (epiece-is-frame easy-piece)
         nil nil))
         
(defun piece-from--easy-piece+synth+synth (easy-piece synth-from synth-to)
  ;; should to maybe
  (piece (epiece-spots easy-piece)
         (epiece-degrees easy-piece)
         (epiece-is-frame easy-piece)
         synth-from synth-to))


(defun piece->easy-piece (piece)
  (easy-piece (piece-spots piece)
              (piece-degrees piece)
              (piece-is-frame piece)))


;;;; function 
(defun synth->maybe-piece (synth-from synth-to)
  (let* 
      ((epiece-from (synth->easy-piece synth-from))
       (epiece-to (synth->easy-piece synth-to))       
       )
    #|
    (piece
     '() '() '()
     synth1 synth2))
    |#
    ))


(defun easy-piece-include-detectuon (easy-piece-from easy-piece-to)
  )

(defun easy-piece-collision-detection (easy-piece1 easy-piece2)
  )




;;;; maybe deploy by synth

(defun synth+synth->maybe-easy-piece (synth-from synth-to)
  ;; transform = rotate . turnover . origin-shift
  (let* 
      ((easy-piece-from (synth->easy-piece synth-from))
       (easy-piece-to (synth->easy-piece synth-to))
       ;;turn
       (turn-over-epiece-list
        (mapcar #'(lambda (is-turn) (turn-orver-easy-piece easy-piece-from is-turn))
                (list t nil)))
       ;; rotate
       (degree-synth-to (vector-angle (spot->vec (nth 1 (epiece-spots easy-piece-to)))))
       (rotate-degree-list
        (mapcar #'(lambda (easy-piece)
                    (vector-angle (spot->vec (nth 1 (epiece-spots easy-piece)))))
                turn-over-epiece-list))
       (maybe-rotate-epiece-list
        (mapcar #'(lambda (easy-piece deg)
                    (maybe-rotate-easy-piece easy-piece 
                                             (- degree-synth-to deg )))
                turn-over-epiece-list rotate-degree-list))
       ;; transform
       (transformed-maybe-epiece (nil=>nothing (car (remove *nothing*
                                                           maybe-rotate-epiece-list))))
       (transformed-list maybe-rotate-epiece-list))
    (transform-to-maybe-easy-piece-list 
     easy-piece-from degree-synth-to
    )))


(defun transform-to-maybe-easy-piece-list (easy-piece next-angle)
    ;; transform = rotate . turnover . origin-shift
  (let* 
      (;;turn
       (turn-over-epiece-list
        (mapcar #'(lambda (is-turn) (turn-orver-easy-piece easy-piece is-turn))
                (list t nil)))
       ;; rotate
       (rotate-degree-list
        (mapcar #'(lambda (easy-piece)
                    (vector-angle (spot->vec (nth 1 (epiece-spots easy-piece)))))
                turn-over-epiece-list))
       (maybe-rotate-epiece-list
        (mapcar #'(lambda (easy-piece deg)
                    (maybe-rotate-easy-piece easy-piece 
                                             (- next-angle deg )))
                turn-over-epiece-list rotate-degree-list))
       ;; transform
       (transformed-maybe-epiece (nil=>nothing (car (remove *nothing*
                                                           maybe-rotate-epiece-list))))
       (transformed-list maybe-rotate-epiece-list))
     transformed-list
  ))


(defun maybe-easy-piece-by-rotate (easy-piece angle)
  (let-maybe
      ((rotate-vecs (list-of-maybe->maybe-list 
                     (mapcar #'(lambda (vec) (real-num-vec->maybe-vec-of-int
                                              (rotate-vec vec angle)))
                             (spots->vecs (epiece-spots easy-piece))))))
    (just (easy-piece
           (vecs->spots rotate-vecs)
           (epiece-degrees easy-piece)
           (epiece-is-frame easy-piece)))))

(defun turn-orver-easy-piece (easy-piece is-turn-over)
  "turn orver easy-piece in y-axis"
  (if is-turn-over
      easy-piece
      (easy-piece
       (mapcar #'(lambda (spot) (spot (- (spot-x spot))
                                      (spot-y spot)))
               (epiece-spots easy-piece))
       (epiece-degrees easy-piece)
       (epiece-is-frame easy-piece))))

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


(defparameter *test-piece-n*
  (spots->piece (list (spot 0 0) (spot 10 0) (spot 10 10) (spot 0 10))))

(defparameter *test-synth-n1*
  (synth *test-piece-n* *plus* 0))

(defparameter *test-synt-n2*
  (synth *test-piece-n* *plus* 0))


(defparameter *test-synthed-piece*
  (synth->maybe-piece *test-synth1* *test-synth2*))


(defparameter *test-easy-piece1*
  (piece->easy-piece *test-piece1*))

(defparameter *test-easy-piece2*
  (piece->easy-piece (nth 2 *test-piece-list1*)))


;;;; call gui

(defun show-easy-piece-list (easy-piece-list)
  (show-piece-list 
   (mapcar #'easy-piece->piece easy-piece-list)))
                        

(defun show-maybe-easy-piece (maybe-easy-piece)
  "<ex> (show-maybe-easy-piece (maybe-easy-piece-by-rotate *TEST-EASY-PIECE1* (* pi 0.5)))"
  (let-maybe ((easy-piece maybe-easy-piece))
    (show-piece (easy-piece->piece easy-piece))))
