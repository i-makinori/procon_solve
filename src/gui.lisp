
(in-package :procon)

;; GUI ;;;;;;;;;;;;;;;;

;;;; gui-piece
(defclass gui-piece ()
  ((name :initarg :name :accessor gui-piece-name)
   (piece :initarg :piece :accessor gui-piece-piece))
  (:default-initargs :name "unnamed" :piece nil))

(defun piece-list->gui-piece-list (piece-list &key (name-rule #'cons-list-n))
  (mapcar #'(lambda (pair)
              (make-instance 'gui-piece
                             :name (car pair)
                             :piece (cdr pair)))
          (funcall name-rule piece-list)))


;;;; clim-util

(defun reset-list-pane (pane items &optional (index 0))
  ;; umm
  (when items
    (setf (climi::visible-items pane) (length items))
    (setf (climi::list-pane-items pane :invoke-callback nil) items)
    (setf (gadget-value pane :invoke-callback t)
          (or (and (slot-boundp pane 'climi::value) (gadget-value pane))
              (let ((values (climi::generic-list-pane-item-values pane)))
                (if (plusp (length values))
                    (elt values index)
                    nil))))))


;;;; frame

(define-application-frame solve-gui ()
  ((gui-piece-list :initarg :gui-piece-list
                   :initform nil
                   :accessor gui-piece-list)
   (synth-list :initform nil 
               :accessor synth-list)
   (current-piece :initform (make-instance 'gui-piece)
                  :accessor current-piece))
  (:panes 
   ;;(interactor :interactor)
   (canvas :application
           :width 600 :height 400
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
                   (gui-piece-list *application-frame*)))


;;;; piece-list-panel

(defun piece-list-pane-changed (pane value) 
  (declare (type gui-piece value)
           (ignore pane))
  
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
    (when piece
      (window-clear stream)

      (format stream "~&~A~%" piece)

      (format stream "piece-label : ~A ~%" name)
      (format stream "is-frame :  ~A ~%" (piece-is-frame piece))
      (format stream "is-nil-piece :  ~A ~%" (is-nil-piece piece))
      (format stream "spots deg : (x y deg) ~% ~A" 
              (mapcar #'(lambda (spot deg)
                          (list (spot-x spot) (spot-y spot) (round (rad->360 deg))))
                      (piece-spots piece) (piece-degrees piece)))
      ;;(format stream "~%~%~%")

      (unless (is-nil-piece piece)
        
        (format stream "~&area : ~A~%" (piece-area piece))
        )
      
      (finish-output stream))))


;;;; display-piece-preview

(defclass canvas 
    (standard-extended-input-stream
     basic-pane
     permanent-medium-sheet-output-mixin)
  ())


(defmethod display-piece-preview (frame stream)
  (let ((medium (sheet-medium stream))
        (piece (gui-piece-piece (current-piece frame))))
    (when (and piece 
               (not (is-nil-piece piece)))
      (let*
          (;; transformation
           ;; all(transform) => (width >= scale * delta-x * 0.5 )
           (delta-x (round (* 0.5 (rectangle-width (sheet-region stream)))))
           (delta-y (round (* 0.5 (rectangle-height (sheet-region stream)))))
           (scale (min (/ (- delta-x 10) (piece-width piece))
                       (/ (- delta-y 10) (piece-height piece))))
           
           (transform (clim:compose-transformations
                       (clim:make-translation-transformation delta-x delta-y)
                       (clim:make-scaling-transformation scale scale))))
        (window-clear stream)
        
        (with-drawing-options
            (medium
             :transformation transform)
          (draw-piece piece stream)
          (draw-coordinate-system piece stream))))))


(defun draw-coordinate-system (piece stream)
  (let* ((axis-length (ceiling (* 0.95 (max (piece-width piece)
                                            (piece-height piece)))))
         (grid-coord-list-1dim 
          (mapcar #'(lambda (num) (* num 5))
                  (upto (floor (/ (- axis-length) 5)) (ceiling (/ axis-length 5))))))

    ;; grid-point
    (if (< axis-length 50)
        (draw-points* stream
                      (flatten (mapcar #'(lambda (y) 
                                           (mapcar #'(lambda (x) (list x y))
                                                   (upto (- axis-length) axis-length)))
                                       (upto (- axis-length) axis-length )))
                      :ink +black+))
    
    ;; grid-line
    (draw-lines* stream
                 (flatten (mapcar #'(lambda (x) 
                                      (list x (- axis-length) x axis-length))
                                  grid-coord-list-1dim))
                 :ink +gray+)
    (draw-lines* stream
                 (flatten (mapcar #'(lambda (y) 
                                      (list (- axis-length) y axis-length y))
                                  grid-coord-list-1dim))
                 :ink +gray+)

    ;; axis-line
    #|
    (draw-line* stream (- axis-length) 0 (* axis-length) 0
    :ink +blue4+ :line-thickness 1)
    (draw-line* stream 0 (- axis-length) 0 (* axis-length)
    :ink +blue3+ :line-thickness 1)
    |#
    ;; origin point
    (draw-point* stream 0 0 :ink +blue+ :line-thickness 10)))

#|
(defun draw-childs-pieces (piece stream)
  (let-maybe
      ((sy-from (just-*-nil=>nothng (piece-synth-from piece)))
       (sy-to (just-*-nil=>nothng (piece-synth-to piece))))
    (let-maybe 
        ((easy-piece-cons 
          (synthesize-able?--also--maybe-consed-easy-piece sy-from sy-to)))
      (draw-easy-piece (car easy-piece-cons) stream)
      (draw-easy-piece (cdr easy-piece-cons) stream)

      (draw-childs-pieces (synth-piece sy-from) stream)
      (draw-childs-pieces (synth-piece sy-to) stream)
      )))
|#
(defun draw-childs-pieces (piece stream)
  (draw-easy-piece (piece->easy-piece piece) stream)
  ;;(let ((piece-synth-fro ))))
)

(defun draw-piece (piece stream)
  (draw-childs-pieces piece stream))

(defun draw-easy-piece (easy-piece stream)
  (draw-polygon* stream
                 (flatten (mapcar #'spot->list (epiece-spots easy-piece)))
                 :filled nil :ink +green+ :line-thickness 3))
