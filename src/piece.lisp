
(in-package :procon)

;;;; piece

(defparameter *nil-piece* 
  (piece nil nil nil nil nil))

(defparameter *zero-piece* nil)

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

(defun piece-area (piece)
  (let ((spots (spots->vecs (piece-spots piece))))
    (reduce #'+ (map-tuple/c #'2vector->area
                             2 spots))))

(defun bad-piece-list->piece-list (bad-piece-list)
  "bad-piece-list :: [piece] && not contain frame-piece"
  (let* ((sorted-pieces (safety-sort 
                         bad-piece-list 
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
