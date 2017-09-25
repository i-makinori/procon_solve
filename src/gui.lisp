
(in-package :procon)

;; GUI

(defun piece->points-consed-list (piece)
  (mapcar #'(lambda (point)
              (cons (rest-assoc :x point)
                    (rest-assoc :y point)))
          (car (rest-assoc :points piece))))

;; for test
(defparameter *test-piece-list* 
  (take *sample-json-1-piece-list* 3))

;;;; run-app

(defun run-solve-gui ()
  (run-frame-top-level (make-application-frame 'solve-gui)))

;;;; frame



(define-application-frame solve-gui ()
  ((piece-list :initform *test-piece-list*
               :accessor piece-list)
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

;;;; panel to draw {piece,synth}

(defmethod display-piece (frame stream)
  (let ((piece-list (piece-list frame)))
    (window-clear stream)
    (mapcar
     #'(lambda (piece)
         (draw-piece piece stream))
     piece-list)
   ))


(defun draw-piece (piece stream)
  (do-tuple/c (p1 p2) 
      (mapcar #'(lambda (point)
                  (cons (* (car point) 10) 
                        (* (cdr point) 10)))
              (piece->points-consed-list piece))
    (draw-line* stream (car p1) (cdr p1) (car p2) (cdr p2))))
  
