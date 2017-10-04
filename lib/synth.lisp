
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

(defun !fix-easy-piece-spot-list (easy-piece)
  ;; bug of easy-piece spots liist
  (let ((spots-fix
         (mapcar #'(lambda (spot?)
                     (if (spot-p spot?) spot? (car spot?)))
                 (epiece-spots easy-piece))))
    (easy-piece spots-fix
                (epiece-degrees easy-piece)
                (epiece-is-frame easy-piece))))

(defun maybe-synthesize-piece (synth1 synth2)
  (let-maybe ((easy-piece-cons
               (synthesize-able?--also--maybe-consed-easy-piece synth1 synth2)))
    ;;(show-easy-piece-list (list (car easy-piece-cons) (cdr easy-piece-cons)))
    (let* ((easy-piece 
            (!fix-easy-piece-spot-list (synthesize-syntesizeable-easy-piece
                                        (car easy-piece-cons) (cdr easy-piece-cons))))
           (is-frame (or (piece-is-frame (synth-piece synth1))
                         (piece-is-frame (synth-piece synth2))))
           (area-test (= (piece-area (easy-piece->piece easy-piece))
                         (+ (piece-area (synth-piece synth1))
                            (piece-area (synth-piece synth2))))))
      (if area-test
          (piece (epiece-spots easy-piece) (epiece-degrees easy-piece)
                 is-frame synth1 synth2)
          (nothing)))))
                      
;;;; synthesize 


(defun synthesize-syntesizeable-easy-piece (able-easy-piece1 able-easy-piece2)
  (let* ((is-frame (or (epiece-is-frame able-easy-piece1)
                       (epiece-is-frame able-easy-piece2)))
         ;; vdr way. maybe in synthesize.lisp
         (easy-piece (synthesize-easy-piece-rule able-easy-piece1 able-easy-piece2))
         ;; csd-way.
         (csd-list (mapcar #'cons
                           (epiece-spots easy-piece)
                           (epiece-degrees easy-piece)))
         (list1 (remove-n-pi-degree (crush-if-serial-of-same-spot csd-list)))
         (list2 (remove-n-pi-degree (crush-if-serial-of-same-spot list1)))
         (vdr-adjust (adjust-vdr-line list2)))
         ;; 
    (if (>= 2 (length vdr-adjust))
        (piece->easy-piece *nil-piece*)
        (easy-piece (mapcar #'car vdr-adjust) (mapcar #'cdr vdr-adjust)
                    is-frame))))

;;;; csd synthesize 

;; adjusts of synthesize by vdr
;; csd-list : Consed-Spots-Degree-list
(labels

    ((c-spot (spot-degree)
       (car spot-degree))
     (c-deg (spot-degree)
       (cdr spot-degree)))

  (let ((n-pi-degree (mapcar #'(lambda (n) (* pi n))
                             (upto 0 10))))
    (defun remove-n-pi-degree (csd-list)
      (remove-if #'(lambda (csd)
                     (find-if #'(lambda (deg) (error-round-deg= (c-deg csd) deg))
                              n-pi-degree))
                 csd-list)))
  
  (defun crush-if-serial-of-same-spot (csd-list)
    (reduce
     #'(lambda (list csd)
         (cond ((null list) (list csd))
               ((spot= (c-spot csd) (c-spot (car list)))
                (cons (cons (c-spot csd) (+ (c-deg csd) (c-deg (car list))))
                      (cdr list)))
               (t (cons csd list))))
     csd-list :initial-value '()))

  (defun adjust-vdr-line (csd-list &optional path)
    "csd-list : consed-spots-degree-list"
    (cond 
      ((null (cdr csd-list)) (append csd-list path))
      ((null path) (adjust-vdr-line (cdr csd-list) (list (car csd-list))))
      ((spot= (c-spot (car path)) (c-spot (cadr csd-list)))
       (adjust-vdr-line-aux (cdr csd-list) path (c-deg (car csd-list))))
      (t (adjust-vdr-line (cdr csd-list) (cons (car csd-list) path)))))

  (defun adjust-vdr-line-aux (csd-list1 csd-list2 ago-cons)
    (cond 
      ((null csd-list1) csd-list2)
      ((null csd-list2) csd-list1)
      ((spot= (c-spot (car csd-list1))
              (c-spot (car csd-list2)))
       (adjust-vdr-line-aux (cdr csd-list1) (cdr csd-list2)
                            (cons (c-spot csd-list1) 
                                  (+ (c-deg (car csd-list1)) (c-deg (car csd-list2))))))
      (t (append csd-list1 (list ago-cons) csd-list2))))
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
  (handler-case 
      (let ((n (search '(()) (epiece-degrees easy-piece)
                        :test #'(lambda (n x) n (> pi x))))
             (vecs (spots->vecs (epiece-spots easy-piece))))
         (gravity-center (list
                          (rotate-nth (- n 1) vecs)
                          (nth n vecs)
                          (rotate-nth (+ n 1) vecs))))
    (type-error (c) ;; caused when 'n' returns nil, such as frame
      (let ((e-piece! (copy-easy-piece easy-piece)))
        (setf (epiece-degrees e-piece!)
              (mapcar #'(lambda (deg) (- 2pi deg))
                      (epiece-degrees easy-piece)))
        (spot-include?-in-easy-piece e-piece!)))))

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



