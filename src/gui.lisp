
(in-package :procon)

;; GUI

(defun piece->points-consed-list (piece)
  (mapcar #'(lambda (point)
              (cons (rest-assoc :x point)
                    (rest-assoc :y point)))
          (car (rest-assoc :points piece))))

;;;; run-app

(defun run-solve-gui ()
  (run-frame-top-level (make-application-frame 'solve-gui)))

;;;; frame



(define-application-frame solve-gui ()
  ((piece-list :initform nil :accessor piece-list)
   (synth-list :initform nil :accessor synth-list)
   (current-piece :initform nil :accessor current-piece))
  ;;(:menu-bar t)
  (:panes 
   (canvas :application
           :height 400 :width 400
           :scroll-bars t
           :display-time t
           :display-function 'display-piece))
  (:layouts 
   (default 
       (vertically ()
         canvas))))

(defmethod display-piece (frame stream)
  (window-clear stream)
  (do-tuple/c (p1 p2) 
      ;;(mapcar #'(lambda (point)
      ;;(cons (* (car point) 10) 
      ;;(* (cdr point) 10)))
      (piece->points-consed-list *sample-piece1*)
    (draw-line* stream (car p1) (cdr p1) (car p2) (cdr p2))
    ))

