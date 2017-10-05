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
