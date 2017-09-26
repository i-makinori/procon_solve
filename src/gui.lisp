
(in-package :procon)

;; GUI

(defun piece->points-consed-list (piece)
  (mapcar #'(lambda (point)
              (cons (rest-assoc :x point)
                    (rest-assoc :y point)))
          (car (rest-assoc :points piece))))

;; for test
(defparameter *test-piece-list* 
  (take *sample-json-1-piece-list* 100))



;;;; gui-piece
(defclass gui-piece ()
  ((name :initarg :name :accessor gui-piece-name)
   (piece :initarg :piece :accessor gui-piece-piece))
  (:default-initargs :name "unnamed" :piece nil))

;; for test
(defparameter *test-gui-piece-list*
  (mapcar #'(lambda (pair)
              (make-instance 'gui-piece
                             :name (car pair)
                             :piece (cdr pair)))
          (cons-list-n *test-piece-list*)))



;;;; run-app

(defvar *solve-gui-state* nil)

(defun run-solve-gui ()
  (setf *solve-gui-state* (make-application-frame 'solve-gui))
  (run-frame-top-level *solve-gui-state*))



;;;; clim-util

(defun reset-list-pane (pane items &optional (index 0))
  ;; umm
  (setf (climi::visible-items pane) (length items))
  (setf (climi::list-pane-items pane :invoke-callback nil) items)
  (setf (gadget-value pane :invoke-callback t)
	(or (and (slot-boundp pane 'climi::value) (gadget-value pane))
	    (let ((values (climi::generic-list-pane-item-values pane)))
	      (if (plusp (length values))
		  (elt values index)
		  nil)))))



;;;; frame

(define-application-frame solve-gui ()
  ((gui-piece-list :initform *test-gui-piece-list*
                   :accessor gui-piece-list)
   (synth-list :initform nil :accessor synth-list)
   (current-piece :initform (make-instance 'gui-piece)
                  :accessor current-piece))
  (:panes 
   (canvas :application
           :scroll-bars t
           :display-time t
           :display-function 'display-piece-preview)
   (piece-list
    (make-pane 'list-pane
               :items nil
               :name-key #'(lambda (gui-piece) (slot-value gui-piece 'name))
               :value-changed-callback 'piece-list-pane-changed)))
  (:layouts 
   (default 
       (horizontally ()
         (vertically (:max-width 200 :min-width nil)
           (labelling (:label "piece-list") (scrolling () piece-list)))
         (vertically (:min-height 200 :min-width 200)
           (labelling (:label "canvas") canvas))))
   (:menu-bar t)
       ))

(defmethod generate-panes :after (fm (frame solve-gui))
  (reset-list-pane (find-pane-named frame 'piece-list)
                   (mapcar #'(lambda (gui-piece)
                               gui-piece)
                           (gui-piece-list *application-frame*))))
           
;;;; piece-list-panel

(defun piece-list-pane-changed (pane value) 
  (declare (type gui-piece value)
           (ignore pane))
  ;;(format t "~&List pane ~A changed to ~S" pane value) ;; for test

  (setf (current-piece *application-frame*) value)
  (display-piece-preview *application-frame*
                         (frame-standard-output *application-frame*))
  )



;;;; display-piece-preview

(defmethod display-piece-preview (frame stream)
  (let* (;; transform
         (scale 2.05)
         (delta-x 200)
         (delta-y 100)

         (transform (clim:compose-transformations
                     (clim:make-translation-transformation delta-x delta-y)
                     (clim:make-scaling-transformation scale scale))))

    (window-clear stream)
    (draw-coordinate-system stream transform)
    (draw-piece (gui-piece-piece (current-piece frame))
                stream
                transform)))


(defun draw-coordinate-system (stream transformation)

  nil
  )

(defun draw-piece (piece stream transformation)
  (draw-polygon* 
   stream
   (flatten 
    (mapcar #'(lambda (point)
                (list (rest-assoc :x point) (rest-assoc :y point)))
            (car (rest-assoc :points piece))))
   :filled nil
   :transformation transformation
    ))
  
