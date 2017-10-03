(in-package :procon)


;;;; run-gui

(defun show-piece-list (piece-list)
  "in CLIM GUI"
  (let ((frame (make-application-frame 
                'solve-gui
                :gui-piece-list (piece-list->gui-piece-list piece-list))))
    (run-frame-top-level frame)))

(defun show-piece (piece)
  (show-piece-list (list piece)))




;;;; easy-piece

(defun show-maybe-consed-easy-piece (maybe-consed-easy-piece)
  (let-maybe ((easy-piece-list maybe-consed-easy-piece))
    (show-easy-piece-list (list (car easy-piece-list) (cdr easy-piece-list)))))


(defun show-easy-piece-list (easy-piece-list)
  (show-piece-list 
   (mapcar #'easy-piece->piece easy-piece-list)))

(defun show-maybe-easy-piece (maybe-easy-piece)
  "<ex> (show-maybe-easy-piece (maybe-easy-piece-by-rotate *TEST-EASY-PIECE1* (* pi 0.5)))"
  (let-maybe ((easy-piece maybe-easy-piece))
    (show-piece (easy-piece->piece easy-piece))))

