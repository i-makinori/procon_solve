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
         (sdl:draw-line-* (+ 320 (x1 line)) (+ 240 (y1 line))
                          (+ 320 (x2 line)) (+ 240 (y2 line)) :color color)))
       lines))
  #|
(defun display-vdr (vdr-list)
  (if vdr-list
      (map-tuple/c
       #'(lambda (vdr1 vdr2)
           (display-def-line (vector-to-line vdr1 vdr2)))
       2 (mapcar #'vdr-vec vdr-list))))

(defun display-vdr-values (vdr-func)
  (multiple-value-bind (vdr1 vdr2) vdr-func
    (display-vdr vdr1)
    (display-vdr vdr2)
    nil
    ))
|#

(defun display-synthesize-vdrs (condi1 condi2)
  (DISPLAY-DATA 
    (multiple-value-bind (vdr1 vdr2) (synthesize-piece condi1 condi2)
      (append (display-vdr vdr1)
              (display-vdr vdr2)))))

(defun display-piece-line (piece)
  (map-tuple/c #'(lambda (v1 v2) 
                   (display-def-line (vector-to-line v1 v2)))
               2  (piece-vectors piece)))

(defun display-piece (piece)
  (display-data
    (display-piece-line piece)))

