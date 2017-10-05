
(in-package :procon)

;;;; piece


(defstruct (piece (:conc-name piece-))
  vecs)

(defun piece (vecs)
  "make-piece"
  (make-piece :vecs vecs))

(defstruct (frame (:conc-name frame-))
  vecs)

(defun frame (vecs)
  "make-frame"
  (make-frame :vecs vecs))


;; parameters

(defun is-nil-piece (piece)
  (>= 2 (length (piece-vecs piece))))
  ;;(null (piece-spots piece)))
  


;;;; util

(defun piece-area (piece)
  (abs (* 0.5 (reduce #'+ (map-tuple #'(lambda (s1 s2) 
                                         (- (* (vx s1) (vy s2))
                                            (* (vy s1) (vx s2))))
                                     2 (piece-vecs piece))))))



(defun bad-piece-list->piece-list (bad-piece-list)
  "bad-piece-list :: [piece] && not contain frame-piece"
  (let* ((to-bad-piece-list 
          (mapcar 
           #'(lambda (piece)
               (piece (mapcar
                       #'(lambda (vecs) ;; spot-origin shift
                           (vec (- (vx (car (piece-vecs piece))) (vx vecs))
                                (- (vy (car (piece-vecs piece))) (vy vecs))))
                       (piece-vecs piece))))
           bad-piece-list))
         ;;
         (sorted-pieces ;; to find frame 
          (safety-sort to-bad-piece-list 
                       #'(lambda (p1 p2) (> (piece-area p1) (piece-area p2)))))
         (frame-piece (car sorted-pieces)))
    (cons frame-piece (cdr sorted-pieces))))
