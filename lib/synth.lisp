
(in-package :procon)

;;;; synthesize


(defun synthesize-piece (frame piece)
  
  )


;;;; synth-list

(defun list-of-synthesizeable (frame piece)
  ;; frame -> piece -> [piece]
  
  )

(defun list-of-synthesize (frame piece)
  "frame -> piece -> [frame]"
  (let* ((transformations
          (list-of-transformation piece))
         )
  )

;;;; deploy

(defstruct (deploy)
  piece
  n-point
  origin
  )

(defun deploy-piece (frame piece)
  
  )

;;;; transform

(defun piece-origin-shift (oringin-vec piece)
  (piece (mapcar #'(lambda (piece-vec)
                     (vec-sub oringin-vec piece-vec))
                 (piece-vecs piece))))

(defun list-of-transformation (piece)
  ;;(list piece (piece->turn-over-piece piece))
  (concat (mapcar #'(lambda (turnover-runc)
                      (piece->rotated-piece-list (funcall turnover-runc piece)))
                  (list #'id #'piece->turn-over-piece))))
  
(defun piece->turn-over-piece (piece)
  ;; in y axis
  (piece (mapcar #'(lambda (vec) (vec (vx vec) (- (vy vec))))
                 (piece-vecs piece))))

(defun piece->rotated-piece-list (piece)
  ;; rotation materix
  (let ((vecs (piece-vecs piece))
        (transform-list
         (list #'(lambda (vec) (vec (+ (vx vec)) ;; 0pi
                                    (+ (vy vec))))
               #'(lambda (vec) (vec (- (vy vec)) ;; 0.5pi
                                    (+ (vx vec))))
               #'(lambda (vec) (vec (- (vx vec)) ;; 1pi
                                    (- (vy vec))))
               #'(lambda (vec) (vec (+ (vy vec)) ;; 1.5pi
                                    (- (vx vec)))))))
    (mapcar #'(lambda (transform)
                (piece (mapcar transform vecs)))
            transform-list)))

(defun test ()
  ;;(show-piece-list (list-of-transformation (nth 1 *test-piece-list1*)))
  (show-piece-list 
   (mapcar #'(lambda (piece) 
               (piece-origin-shift (vec 10 10) piece))
           (list-of-transformation (nth 1 *test-piece-list1*)))
  ))
