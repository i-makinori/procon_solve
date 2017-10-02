
(in-package :procon)

;;;; synth-piece ;;;;;;;;;;;;;;;;

;;;; struct 

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

(defun piece->easy-piece (piece)
  (easy-piece (piece-spots piece)
              (piece-degrees piece)
              (piece-is-frame piece)))

(defun piece-from--easy-piece+synth+synth (easy-piece synth-from synth-to)
  ;; should to maybe
  (piece (epiece-spots easy-piece)
         (epiece-degrees easy-piece)
         (epiece-is-frame easy-piece)
         synth-from synth-to))



;;;; function 

(defun maybe-synthesize-piece (synth1 synth2)
  (let-maybe ((easy-piece-cons
               (synthesize-able?--also--maybe-consed-easy-piece synth1 synth2)))
    ;;(show-easy-piece-list (list (car easy-piece-cons) (cdr easy-piece-cons)))
    (let* ((easy-piece (synthesize-syntesizeable-easy-piece
                        (car easy-piece-cons) (cdr easy-piece-cons)))
           (is-frame (or (piece-is-frame (synth-piece synth1))
                         (piece-is-frame (synth-piece synth2)))))
      (just 
       (piece (epiece-spots easy-piece) (epiece-degrees easy-piece)
              is-frame synth1 synth2)))))
                      
;;;; synthesize 


(defun synthesize-syntesizeable-easy-piece (able-easy-piece1 able-easy-piece2)
  (let* (;; vdr way. maybe in synthesize.lisp
         (easy-piece-d (synthesize-easy-piece-rule able-easy-piece1 able-easy-piece2))
         (easy-piece (if easy-piece-d easy-piece-d 
                         (piece->easy-piece *nil-piece*)))
         ;; csd-way.
         (csd-list (mapcar #'cons
                           (epiece-spots easy-piece)
                           (epiece-degrees easy-piece)))
         (list1 (vdr-adjust1 csd-list))
         (vdr-adjust list1))
         ;; 
    (if (>= 2 (length vdr-adjust))
        (piece->easy-piece *nil-piece*)
        (easy-piece (mapcar #'car vdr-adjust)
                    (mapcar #'cdr vdr-adjust)
                    nil))))

(defun test ()
  (show-piece-list (synthesize-piece-list-all *test-piece1* *test-piece1*)))



;;;; csd synthesize 

;; adjusts of synthesize by vdr
;; csd-list : Consed-Spots-Degree-list
(labels
    ((c-spot (spot-degree)
       (car spot-degree))
     (c-deg (spot-degree)
       (cdr spot-degree)))

  #|
  (defun vdr-adjust (list)
    ;; its too slow :: O((2*n)^2)
    (let ((adjust (vdr-adjust1 (vdr-adjust2 list))))
      ;; length form
      ))

  (defun vdr-adjust1 (list &optional path)
    (cond ((null (cdr list)) (cons (car list) path))
          ((null path)
           (vdr-adjust1 (cdr list) (cons (car list) path)))
          ((spot= (c-spot (car list)) (c-spot (car path)))
           (vdr-adjust1 (cddr list) (cdr path)))
          (t
           (vdr-adjust1 (cdr list) (cons (car list) path)))))
  |#

  (defun vdr-adjust1 (list &optional path)
    (cond ((null (cdr list)) (cons (car list) path))
          ((null path)
           (vdr-adjust1 (cdr list) (cons (car list) path)))
          ((spot= (c-spot (car list)) (c-spot (car path)))
           (print "hoge")
           (vdr-adjust1 (vdr-adjust2 (cdr list) path)))
          (t
           (vdr-adjust1 (cdr list) (cons (car list) path)))))
  
  (defun vdr-adjust2 (list &optional path)
    (cond ((null (cdr list)) (cons (car list) path))
          ((null (car list)) 
           (vdr-adjust2 (cdr list) (cons (car list) path)))
          ((not (spot= (c-spot (car list)) (c-spot (cadr list))))
           (vdr-adjust2 (cdr list) (cons (car list) path)))
          (t 
           (vdr-adjust2 (cons (car path) (cddr list)) (cdr path)))))
  )
  

;;;; synthesizeable

(defun synthesize-able?-easy-piece (easy-piece-from easy-piece-to)
  (let ((collision (easy-piece-collision-detection easy-piece-from easy-piece-to))
        (include (easy-piece-include-detectuon easy-piece-from easy-piece-to))
        (frame-p (or (epiece-is-frame easy-piece-from)
                     (epiece-is-frame easy-piece-to))))
    (cond (collision nil)
          ((and include frame-p) t)
          ((and (not include) (not frame-p)) t)
          (t nil))))


(defun synthesize-able?--also--maybe-consed-easy-piece (synth1 synth2)
  (let-maybe 
      ((deployed-piece-list1 (synth+synth->maybe-deployed-easy-piece-list synth1 synth2))
       (deployed-piece2 (just (synth->easy-piece synth2))))
    (let-maybe
        ((deployed-piece1 (just-*-nil=>nothng 
                           (car (remove-if-not
                                 #'(lambda (epiece) (synthesize-able?-easy-piece
                                                     epiece deployed-piece2))
                                 deployed-piece-list1)))))
      (just (cons deployed-piece1 deployed-piece2)))))


;;;; collision detection

(defun easy-piece->line-list (easy-piece)
  (map-tuple/c #'vector-to-line 2 
               (spots->vecs (epiece-spots easy-piece))))


(defun easy-piece-include-detectuon (easy-piece1 easy-piece2)
  "piece16- hit-judge"
  (let ((lines1 (easy-piece->line-list easy-piece1))
        (lines2 (easy-piece->line-list easy-piece2)))
    (or
     (spot-vec-include?-in-piece-line-list 
      lines2 (spot-include?-in-easy-piece easy-piece1))
     (spot-vec-include?-in-piece-line-list
      lines1 (spot-include?-in-easy-piece easy-piece2)))))


(defun spot-vec-include?-in-piece-line-list (piece-line-list spot-vec)
  "nil : not-included
t : included"
  (let ((judged-line1 (vector-to-line (vec *-huge-num* (vy spot-vec)) 
                                      spot-vec))
        (judged-line2 (vector-to-line (vec *huge-num*  (1+ (vy spot-vec)))
                                      (vec (+ 2 (vx spot-vec)) (+ 2 (vy spot-vec))))))
    (not (some #'(lambda (line) 
                   (evenp (length (remove nil
                                          (mapcar #'(lambda (l)
                                                      (line-collision-detection l line))
                                                  piece-line-list)))))
               (list judged-line1 judged-line2)))))


(defun spot-include?-in-easy-piece (easy-piece)
  "search each triangle included in easy-piece,
center-deg is smaller than (pi - st-error), it's gravity-center is included in piece16"
  (let ((n (search '(()) (epiece-degrees easy-piece)
                   :test #'(lambda (n x) n (> pi x))))
        (vecs (spots->vecs (epiece-spots easy-piece))))
    (gravity-center (list
                     (rotate-nth (- n 1) vecs)
                     (nth n vecs)
                     (rotate-nth (+ n 1) vecs)))))

(defun easy-piece-collision-detection (easy-piece1 easy-piece2)
  "nil=>not hit, T=>hit"
  (let ((lines1 (easy-piece->line-list easy-piece1))
        (lines2 (easy-piece->line-list easy-piece2)))
    (lines-lines-hit-judge lines1 lines2)))

(defun lines-lines-hit-judge (lines1 lines2)
  (not (every #'(lambda (line2)
                  (notany #'(lambda (line1)
                              (line-collision-detection-error line1 line2))
                          lines1))
              lines2)))


;;;; maybe deploy by synth

(defun synth+synth->maybe-deployed-easy-piece-list (synth-from synth-to)
  "deploy maybe easy-piece-list to coordinate matrix"
  (let* ((easy-piece-from (synth->easy-piece synth-from))
         (easy-piece-to (synth->easy-piece synth-to))
         (degree-synth-to (vector-angle (spot->vec (nth 1 (epiece-spots easy-piece-to))))))
    (transform-to-maybe-easy-piece-list
     easy-piece-from degree-synth-to)))

(defun transform-to-maybe-easy-piece-list (easy-piece next-angle)
  "deploy maybe easy-piece-list to coordinate matrix"
  ;; transform = rotate . turnover . origin-shift
  (let* 
      ((turn-over-epiece-list
        (mapcar #'(lambda (is-turn) (turn-orver-easy-piece easy-piece is-turn))
                (list t nil)))
       (rotate-degree-list
        (mapcar #'(lambda (easy-piece)
                    (vector-angle (spot->vec (nth 1 (epiece-spots easy-piece)))))
                turn-over-epiece-list))
       (maybe-rotated-epiece-list
        (mapcar #'(lambda (easy-piece deg)
                    (maybe-easy-piece-by-rotate easy-piece 
                                                (- next-angle deg )))
                turn-over-epiece-list rotate-degree-list)))
    (list-of-maybe->maybe-list
     (remove *nothing* maybe-rotated-epiece-list))))

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

(defparameter *test-easy-piece1*
  (piece->easy-piece *test-piece1*))

(defparameter *test-easy-piece2*
  (piece->easy-piece (nth 2 *test-piece-list1*)))


(defparameter *test-synth1* 
  (synth *test-piece-for-synth1* *plus* 5))

(defparameter *test-synth2*
  (synth *test-piece-for-synth2* *minus* 4))


(defparameter *test-piece-n1*
  (spots->piece (list (spot 0 0) (spot 10 0) (spot 10 10) (spot 0 10))))

(defparameter *test-piece-n2*
  (let ((piece (spots->piece (list (spot 0 0) (spot 0 20) (spot 10 20) (spot 20 0)))))
    (piece (piece-spots piece)
           (piece-degrees piece)
           t nil nil)))

(defparameter *test-synth-n1*
  (synth *test-piece-n1* *plus* 1))

(defparameter *test-synth-n2*
  (synth *test-piece-n2* *minus* 0))



;;;; call gui

(defun show-easy-piece-list (easy-piece-list)
  (show-piece-list 
   (mapcar #'easy-piece->piece easy-piece-list)))

(defun show-maybe-easy-piece (maybe-easy-piece)
  "<ex> (show-maybe-easy-piece (maybe-easy-piece-by-rotate *TEST-EASY-PIECE1* (* pi 0.5)))"
  (let-maybe ((easy-piece maybe-easy-piece))
    (show-piece (easy-piece->piece easy-piece))))

(defun show-maybe-consed-easy-piece (maybe-consed-easy-piece)
  (let-maybe ((easy-piece-list maybe-consed-easy-piece))
    (show-easy-piece-list (list (car easy-piece-list) (cdr easy-piece-list)))))
