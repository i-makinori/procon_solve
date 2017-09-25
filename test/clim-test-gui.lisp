
;;;; test-gui

(ql:quickload :mcclim)
(in-package :clim-user)

(define-application-frame test-gui ()
  ((greeting :initform "hoge"
             :accessor greeting))
  (:pane (make-pane 'test-pane)))

(defclass test-pane (clim-stream-pane)
  ())

(defmethod handle-repaint ((pane test-pane) region)
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    (draw-rectangle* pane 0 0 w h
                     :filled t
                     :ink (pane-background pane))
    (draw-text* pane
                (greeting *application-frame*)
                (floor w 2) (floor h 2)
                :align-x :center
                :align-y :center)))

(defun run-test-gui ()
  (run-frame-top-level (make-application-frame 'test-gui)))
              
