
(in-package :procon)

;;;; piece

(defparameter *its-frame* T)
(defparameter *its-piece* nil)

(defstruct (piece (:conc-name piece-))
  spots degrees is-frame synth-from synth-to)

(defun piece (spots degrees is-frame synth-from synth-to)
  "make-piece"
  (make-piece :spots spots
              :degrees degrees
              :is-frame is-frame
              :synth-from synth-from
              :synth-to synth-to))

(defun spots->piece (spots)
  (let ((degrees (degree-adjust
                  (map-tuple/c #'clock-wise-angle 3
                               (rotate-list (spots->vecs spots) -1)))))
    (piece
     spots degrees nil nil nil)))

;; parameters
(defparameter *nil-piece*
  (piece nil nil nil nil nil))

(defun is-nil-piece (piece)
  (>= 2 (length (piece-spots piece)))
  ;;(null (piece-spots piece)))
  )

(defparameter *zero-piece* nil)


;;;; util

(defun piece-able-states (piece)
  ;; list of piece of conversion possible.
  ;; which is set of discrete spots, flip overd of it, and so on.
  ;;
  ;; (rotate(spots) => [spots]) 
  ;; .[poins] => all spots are included in discrete spot.
  )


(defun piece->piece-s-not-frame-piece (piece)
  "ret-piece.is-frame <- not piece.is-frame"
  (piece (piece-spots piece) (piece-degrees piece)
         (not (piece-is-frame piece))
         (piece-synth-from piece) (piece-synth-to piece)))

(defun degree-adjust (degrees)
  "in n-polygon. when degree-rength is not (n-2)*pi, let degree-length (n-2)*pi "
  (cond ((error-round-deg= pi
                           (/ (reduce #'+ degrees) (- (length degrees) 2)))
         degrees)
        (t (mapcar #'(lambda (deg) (- 2pi deg)) degrees))))

(defun piece-area (piece)
  (let ((area-dir ;; piece-is-frame => (area<=0) , !=> (area>=0)
         (if (piece-is-frame piece) #'- #'+)))
    (funcall area-dir
             (* 0.5 (reduce #'+ 
                            (map-tuple #'(lambda (s1 s2) (* (- (spot-x s1) (spot-x s2))
                                                            (+ (spot-y s1) (spot-y s2))))
                                       2 (piece-spots piece)))))))


(defun bad-piece-list->piece-list (bad-piece-list)
  "bad-piece-list :: [piece] && not contain frame-piece"
  (let* ((to-bad-piece-list 
          (mapcar #'(lambda (piece)
                      (piece (piece-spots piece) (piece-degrees piece) nil nil nil))
                  bad-piece-list))
         (sorted-pieces (safety-sort 
                         to-bad-piece-list 
                         #'(lambda (p1 p2) (> (piece-area p1) (piece-area p2)))))
         (bad-frame-piece (car sorted-pieces))
         (frame-piece (piece (piece-spots bad-frame-piece)
                             (mapcar #'(lambda (deg) (- 2pi deg))
                                     (piece-degrees bad-frame-piece))
                             t nil nil)))
    (cons frame-piece (cdr sorted-pieces))))


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
