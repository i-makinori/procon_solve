(in-package #:procon)


;;;; display functions

(defparameter *display-lines* '())

(defmacro display-data (&body doings)
  `(sdl:with-init ()
     (sdl:window 640 480)
     (setf (sdl:frame-rate) 30)
     (setf *display-lines*
           (append ,@doings *display-lines*))
     (sdl:with-events ()
       (:quit-event () (setf *display-lines* nil) t)
       (:idle
        ()
        (sdl:clear-display  sdl:*black*)
        (sdl:draw-filled-circle-* 320 240 3 :color sdl:*red*)
        (display-lines *display-lines*)
        (sdl:update-display)))))

(defun display-def-line (line &optional (color sdl:*white*))
  (cons line color))

(defun display-lines (lines)
  (mapcar
   #'(lambda (dl)
       (let ((line (car dl))
             (color (cdr dl)))
         (sdl:draw-line-* (+ 320 (round (/ (x1 line) 40)))
                          (+ 240 (round (/ (y1 line) 40)))
                          (+ 320 (round (/ (x2 line) 40)))
                          (+ 240 (round (/ (y2 line) 40)))
                          :color color)))
       lines))

(defun display-piece-line (piece)
  (map-tuple/c #'(lambda (v1 v2) 
                   (display-def-line (vector-to-line v1 v2)))
               2  (piece-vectors piece)))

(defun display-piece (piece)
  (display-data
    (display-piece-line piece)))

#|
(defun display-piece-list (piece-list)
  (display-data
    (append (mapcar #'display-piece-line
                    piece-list))))
|#
