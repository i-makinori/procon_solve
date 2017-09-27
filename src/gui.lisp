
(in-package :procon)

;; GUI

(defun piece->points-consed-list (piece)
  (mapcar #'(lambda (point)
              (cons (rest-assoc :x point)
                    (rest-assoc :y point)))
          (car-rest-assoc :points piece)))

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

(defun run-solve-gui (piece-list)
  (let ((frame (make-application-frame 'solve-gui
                                       :gui-piece-list piece-list)))
    (run-frame-top-level frame)))

;; for test
(defvar *solve-gui-state* nil)

(defun run-solve-gui-test ()
  (setf *solve-gui-state*
        (make-application-frame 'solve-gui
                                :gui-piece-list *test-gui-piece-list*))
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
   ;;(interactor :interactor)
   (canvas :application
           :width 400 :height 400
           :scroll-bars t
           :display-function 'display-piece-preview)
   (current-piece-info :application
                       :max-height 150
                       :display-time nil
                       :text-style (make-text-style :sans-serif :roman :normal)
                       :display-function #'write-piece-info)
    
   (piece-list
    (make-pane 'list-pane
               :items nil
               :name-key #'(lambda (gui-piece) (slot-value gui-piece 'name))
               :value-changed-callback 'piece-list-pane-changed)))
  (:layouts 
   (default 
       (vertically ()
         (horizontally ()
           (vertically (:max-width 200 :min-width nil)
             (labelling (:label "piece-list") 
               (scrolling () piece-list)))

           (labelling (:label "piece-info")
             (vertically ()
               canvas
               current-piece-info
             )))
         ;;interactor
         )))
  (:menu-bar t))

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

  (display-piece-preview
   (pane-frame (find-pane-named *application-frame* 'canvas))
   (get-frame-pane *application-frame* 'canvas))

  (write-piece-info
   (pane-frame (find-pane-named *application-frame* 'current-piece-info))
   (get-frame-pane *application-frame* 'current-piece-info)))

;;;; write-piece-info
(defmethod write-piece-info (frame stream)
  (let ((name (gui-piece-name (current-piece frame)))
        (piece (gui-piece-piece (current-piece frame))))

    (window-clear stream)
    (format stream "You Selected piece : ~A ~%" name)
    (finish-output stream)))

;;;; display-piece-preview

(defmethod display-piece-preview (frame stream)
  (let* ((medium (sheet-medium stream))
         (piece (gui-piece-piece (current-piece frame)))
         ;; transformation
         ;; all(transform) => (width >= scale * delta-x * 0.5 )
         (delta-x (round (* 0.5 (rectangle-width (sheet-region stream)))))
         (delta-y (round (* 0.5 (rectangle-height (sheet-region stream)))))
         (scale (min (/ (* 0.95 delta-x) (piece-width piece))
                     (/ (* 0.95 delta-y) (piece-height piece))))

         (transform (clim:compose-transformations
                     (clim:make-translation-transformation delta-x delta-y)
                     (clim:make-scaling-transformation scale scale))))

    (window-clear stream)

    (with-drawing-options
        (medium
         :transformation transform)
      (draw-coordinate-system piece stream)
      (draw-piece piece
                  stream))))


(defun draw-coordinate-system (piece stream)
  (let ((axis-length (round (* 0.95 (max (piece-width piece)
                                         (piece-height piece))))))
    
    ;; axis-line
    (draw-line* stream (- axis-length) 0 (* axis-length) 0
                :ink +blue+)
    (draw-line* stream 0 (- axis-length) 0 (* axis-length)
                :ink +blue+)

    ;; grid
    (draw-points* 
     stream
     (flatten (mapcar #'(lambda (y) 
                          (mapcar #'(lambda (x) (list x y))
                                  (upto (- axis-length) axis-length)))
                      (upto (- axis-length) axis-length ))))))

(defun draw-piece (piece stream)
  (draw-polygon* 
   stream
   (flatten 
    (mapcar #'(lambda (point)
                (list (rest-assoc :x point) (rest-assoc :y point)))
            (car (rest-assoc :points piece))))
   :filled nil))

