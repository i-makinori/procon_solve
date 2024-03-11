(in-package :puzzle-1617)

;;;; partial problem
;;; such as line or angle




;; 1. all of length of frame segment has equall length by the synthesize of other pieces.
;;    ∀(x±1) . (x±1) is frame_segment
;;    => ∃N . Length((x±1)) = Σ Length(piece_segment_(ni±1)) while N={n1,n2,...,nn}
;; 2. all angle of all points has 360-degree synthesize with other pieces.
;;    ∀x. x is piece_point
;;    => ∃N. Σ Angle(piece_angle_ni) while N={n1,n2,...,nn} = 2*π


;; 部分問題を先に解く
;; 部分問題を解けない頂点や辺を起点とする合成は行わないことで、計算を削減する。

;;; defines and initialize


;;;

(defparameter *memo-of-partial-problem*
  nil)

(defparameter *depth-const-of-partial-problem*
  3)

(defstruct (value-and-sy-param (:constructor make-vvsy) (:conc-name vvsy-))
  (:angle 1000000) (:length^2 1000000) ;; value value
  (:id -1) (:nc 0) (:pm 0)) ;; sy-select parameter

(defun partial-value-sy-param-list (piece)
  (let ((id (piece-id piece))
        (sy-param-list
          (whole-set-of-point-and-edge-selections-piece piece)))
    (mapcar
     #'(lambda (sy-param) ;; [nth_point, pm_sign]
         (let* ((nc (nth 0 sy-param)) (pm (nth 1 sy-param))
                (angle    (modnth (+ nc 0)
                                  (shape-angle-list (piece-shape piece))))
                (length^2 (modnth (+ nc (if (eq pm +1) 0 -1))
                                  (shape-segment-length^2-list (piece-shape piece)))))
           (make-vvsy :angle angle :length^2 length^2 :id id :nc nc :pm pm)))
     sy-param-list)))
  
(defun sy-select-parameters-from-piece-list (piece-list)
  (flatten (mapcar #'partial-value-sy-param-list piece-list)))


;;;

;;;

#|
(defun solve-partial-problem-aux (objective-value choice-value-list
                                  this-depth-queue next-depth-queue solutions current-depth)
  (cond ((>= current-depth *depth-const-of-partial-problem*)
         solutions)
        ((null this-depth-queue)
         (solve-partial-problem-aux objective-value choice-value-list
                                    next-depth-queue '() solutions (+ 1 current-depth)))
        (t
         (let* ((step-vs (car this-depth-queue))
                (step-next-depth-queue-combination
                  (mapcar #'(lambda (v) (cons v step-vs)) ;; it is unneeded to remove equally set
                          choice-value-list))
                (step-next-depth-queue
                  (remove-if-not #'(lambda (vs) (ser<= (reduce #'+ vs :initial-value 0)
                                                       objective-value))
                                 step-next-depth-queue-combination))
                (step-solutions
                  (remove-if-not #'(lambda (vs) (ser= (reduce #'+ vs :initial-value 0)
                                                      objective-value))
                                 step-next-depth-queue-combination)))
           #|
           (format t "depth ~A. ~A = Sum ,~A~%"
                   current-depth
                   (reduce #'+ step-vs :initial-value 0)
                   step-vs)
           |#        
           (solve-partial-problem-aux
            objective-value choice-value-list
            (cdr this-depth-queue)
            (append next-depth-queue step-next-depth-queue)
            (append solutions step-solutions)
            current-depth)
           ))))
|#

(defun solve-partial-problem-aux (objective-value choice-value-list
                                  this-depth-queue next-depth-queue solutions current-depth)
  (cond ((> current-depth *depth-const-of-partial-problem*)
         solutions)
        ((null this-depth-queue)
         (solve-partial-problem-aux objective-value choice-value-list
                                    next-depth-queue '() solutions (+ 1 current-depth)))
        (t
         (let* ((step-vs (car this-depth-queue))
                (step-value (apply #'+ step-vs)))
           (cond 
             ((ser= step-value objective-value)
              (solve-partial-problem-aux 
               objective-value choice-value-list
               (cdr this-depth-queue) next-depth-queue
               (cons (car this-depth-queue) solutions) current-depth))
             ((ser> step-value objective-value)
              (solve-partial-problem-aux
               objective-value choice-value-list
               (cdr this-depth-queue) next-depth-queue
               solutions current-depth))
             (t
              (solve-partial-problem-aux
               objective-value choice-value-list
               (cdr this-depth-queue)
               (append  next-depth-queue
                        (mapcar #'(lambda (v) (cons v step-vs)) choice-value-list))
               solutions current-depth)))))))

                                    
(defun solve-partial-problem (objective-value choice-value-list)
  (remove-equally-set-from-set-list
   (solve-partial-problem-aux objective-value choice-value-list
                              (mapcar #'list choice-value-list)
                              '() '() -1)))


(defun solve-partial-angle (vvsy available-piece-list)
  (solve-partial-problem (- *pi*2* (vvsy-angle vvsy))
                         (flatten (mapcar #'piece-angle-list available-piece-list)))
  ;;flatten (mapcar #'piece-angle-list available-piece-list))
  )


;;;

#|
(defun solve-partial-angle-aux (vvsy-list queue-current queue-next
                                memo-structure current-depth)
  ;;(format t "~A~%" (car queue-current))
  (cond 
    ((>= current-depth *depth-const-of-partial-problem*)
     memo-structure)
    ((null queue-current)
     (format t "next-len: ~A~%" (length queue-next))
     (solve-partial-angle-aux vvsy-list
                              (identity queue-next ) ;; todo unique set
                              '()
                              memo-structure (1+ current-depth)))
    (t
     (let* ((step-vvsy-s (car queue-current))
            (step-angle (reduce #'(lambda (val vvsy) (+ val (vvsy-angle vvsy)))
                                step-vvsy-s :initial-value 0)))
       ;;
       #|
       (format t "~A, ~6,f, " (length step-vvsy-s) step-angle)
       (format t "~A~%"
               (mapcar #'(lambda (v)
                           ;;(format nil "[~A,~A,~A] " (vvsy-id v) (vvsy-nc v) (vvsy-pm v)))
                           (format nil "~A " (vvsy-id v)))
                           step-vvsy-s))
       |#
       ;;
       (cond ((ser= step-angle *pi*2*)
              (solve-partial-angle-aux vvsy-list (cdr queue-current) queue-next
                                       (cons step-vvsy-s memo-structure) current-depth))
             ((ser> step-angle *pi*2*)
              (solve-partial-angle-aux vvsy-list (cdr queue-current) queue-next
                                       memo-structure current-depth))
             (t
              (let ((nexts-which-uses-this-step
                      (mapcar
                       #'(lambda (vvsy) (cons vvsy step-vvsy-s))
                       (remove-if 
                        #'(lambda (vvsy)
                            (find-if #'(lambda (s_n) (equal (vvsy-id s_n) (vvsy-id vvsy)))
                                     step-vvsy-s))
                        vvsy-list))))
                (solve-partial-angle-aux vvsy-list (cdr queue-current)
                                         (append queue-next nexts-which-uses-this-step)
                                         memo-structure
                                         current-depth)))             
             )))))
|#
#|
(defun solve-partial-angle-aux (vvsy-list queue-current queue-next
                                memo-structure current-depth)
  ;;(format t "~A~%" (car queue-current))
  (cond 
    ((>= current-depth *depth-const-of-partial-problem*)
     memo-structure)
    ((null queue-current)
     (format t "next-len: ~A~%" (length queue-next))
     (solve-partial-angle-aux vvsy-list
                              (identity queue-next ) ;; todo unique set
                              '()
                              memo-structure (1+ current-depth)))
    (t
     (let* ((step-vvsy-s (car queue-current))
            (step-angle (reduce #'(lambda (val vvsy) (+ val (vvsy-angle vvsy)))
                                step-vvsy-s :initial-value 0)))
       #|
       (cond ((ser= step-angle *pi*2*)
              (solve-partial-angle-aux vvsy-list (cdr queue-current) queue-next
                                       (cons step-vvsy-s memo-structure) current-depth))
             ((ser> step-angle *pi*2*)
              (solve-partial-angle-aux vvsy-list (cdr queue-current) queue-next
                                       memo-structure current-depth))

             (t
|#
       (let ((nexts-which-uses-this-step
               (mapcar
                #'(lambda (vvsy) (cons vvsy step-vvsy-s))
                (remove-if 
                 #'(lambda (vvsy)
                     (find-if #'(lambda (s_n) (equal (vvsy-id s_n) (vvsy-id vvsy)))
                              step-vvsy-s))
                 vvsy-list))))
         (solve-partial-angle-aux vvsy-list (cdr queue-current)
                                  (append queue-next nexts-which-uses-this-step)
                                  memo-structure
                                  current-depth)))
     
     )))
|#

#|
(defun solve-partial-angle (frame-piece primary-piece-list)
  (let* ((remain-pieces (list-of-unused-primary-piece-list-of-synthesized-piece
                         frame-piece primary-piece-list)))
    (solve-partial-angle-aux 
     ;;(sy-select-parameters-from-piece-list 
     ;;(append (list frame-piece) remain-pieces)) ;; todo
     (sy-select-parameters-from-piece-list remain-pieces)
     (mapcar #'list (sy-select-parameters-from-piece-list (list frame-piece)))
     '()
     '()
     0)))
|#
