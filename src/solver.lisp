
(in-package :puzzle-1617)

;;;; solver

;;; all synthesizeable patterns

(defun all-synthesizeable-patterns-of-pieces-to-frame (frame piece-list)
  (flatten
   (remove nil 
           (mapcar #'synthesize-piece-to-frame-by-selection-piece-or-fail
                   (whole-set-of-point-and-edge-selections-pieces-to-frame
                    frame piece-list)))))


;;; 
;;; set theoretical manipulations

(defun primary-piece-p (piece-common)
  ;; piece- null?
  (eq 'leaf (piece-leaf-or-synthed piece-common)))

(defun list-of-primary-piece-list-of-synthesized-piece (synthesized-piece)
  (labels ((aux (p_n)
             (cond ((primary-piece-p p_n) (list p_n))
                   (t (append
                       (if (piece-transform1 p_n)
                           (aux (transform-piece (piece-transform1 p_n)))
                           nil)
                       (if (piece-transform2 p_n)
                           (aux (transform-piece (piece-transform2 p_n)))
                           nil))))))
    (aux synthesized-piece)))

(defun list-of-piece-of-synthesized-piece (synthesized-piece)
  (labels ((aux (p_n)
             (cond ((null p_n) nil)
                   (t (append
                       ;;(list (piece-id p_n))
                       (list p_n)
                       (if (piece-transform1 p_n)
                           (aux (transform-piece (piece-transform1 p_n)))
                           nil)
                       (if (piece-transform2 p_n)
                           (aux (transform-piece (piece-transform2 p_n)))
                           nil))))))
    (aux synthesized-piece)))

(defun list-of-unused-primary-piece-list-of-synthesized-piece (synthesized-piece primary-piece-list)
  (set-difference ;; set of (original - used)
   primary-piece-list
   (list-of-primary-piece-list-of-synthesized-piece 
    synthesized-piece)
   :test #'(lambda (p1 p2) (equal (piece-id p1) (piece-id p2)))))


;;; sort by delta points

(defun sort-by-delta_points (synthesized-piece-list)
  (sort synthesized-piece-list
        #'(lambda (p1 p2)
            (> (delta_points-of-synthesize p1)
               (delta_points-of-synthesize p2)))))

(defun delta_points-of-synthesize (synthesized-piece)
  (let ((n-points-of-primary
         (apply #'+ (mapcar #'(lambda (piece) (length (piece-coord-points piece)))
                     (list-of-primary-piece-list-of-synthesized-piece synthesized-piece))))
        (n-points-of-synthesized-piece
          (length (piece-coord-points synthesized-piece))))
    (- n-points-of-primary n-points-of-synthesized-piece)))

(defun remove-congruent-from-synthesized-piece-list (synthesized-piece-list)
  (labels ((aux (lis)
             (cond ((null lis) '())
                   (t ;;(format t "~A:~%" (piece-id (car lis)))
                    (cons (car lis)
                          (aux
                           (remove-if #'(lambda (p) (detect-piece-congruent (car lis) p))
                                      (cdr lis))))))))
    (aux synthesized-piece-list)))


;;; evaluation functions for evaluate values

(defun evaluation-value-by-delta-points (state primary-piece-list)
  primary-piece-list
  (delta_points-of-synthesize (assocdr :frame state)))

(defun evaluation-value-by-div1-nomials (state primary-piece-list)
  ;;(let* ((synth-prims (list-of-primary-piece-list-of-synthesized-piece (assocdr :frame state))))
  ;;primaries
  primary-piece-list
  state
  0)

(defun sorted-states-by-evaluation-function (evaluation-function state-list! primary-piece-list)
  (let ((states-added-evaluation-value
          (mapcar #'(lambda (state)
                      (if (null (assocdr :evaluation-value state))
                          (cons `(:evaluation-value 
                                  . ,(funcall evaluation-function state primary-piece-list))
                                state)
                          state))
                  state-list!)))
    (sort
     states-added-evaluation-value
     #'(lambda (state1 state2)
         (> (assocdr :evaluation-value state1)
            (assocdr :evaluation-value state2))))))

;;;

#|
;;;; test
(write-piece-list-as-html
              (all-synthesizeable-patterns-of-pieces-to-frame
               (car *example-problem-9*) (cdr *example-problem-9*)))


(write-piece-list-as-html
 (sort-by-delta_points
  (all-synthesizeable-patterns-of-pieces-to-frame
   (car *example-problem-9*) (cdr *example-problem-9*))))


|#


;;;; search solution

;; minus to frame method

#|
;; usage
(search-solution-from-prime-pieces
 (cons (car *example-problem-9*) (cdr *example-problem-9*)))
|#

(defun search-solution-aux (stack-of-states primary-piece-list)
  (let* ((state (car stack-of-states))
         (stacking (cdr stack-of-states))
         ;;
         (piece-frame         (assocdr :frame state))
         (primary-piece-using (list-of-primary-piece-list-of-synthesized-piece piece-frame))
         (primary-piece-rests (list-of-unused-primary-piece-list-of-synthesized-piece 
                               piece-frame primary-piece-list))
         ;;
         )
    (format t "synth-list to: ~A, using-pieces [len]: ~A[~A]~%"
            (piece-id piece-frame)
            (mapcar #'piece-id primary-piece-using)
            (length primary-piece-using))
    (let* ((patterns-of-step
             (remove-congruent-from-synthesized-piece-list
              (sort-by-delta_points
               (all-synthesizeable-patterns-of-pieces-to-frame
                piece-frame primary-piece-rests))))
           (states-of-step! (mapcar #'(lambda (next-frame)
                                        `((:frame . ,next-frame)))
                                    patterns-of-step))
           (next-stack (sorted-states-by-evaluation-function
                        #'evaluation-value-by-delta-points
                        (append states-of-step! stacking)
                        primary-piece-list)))
      ;; debugging format
      (format t "~A" (mapcar #'(lambda (s) (assocdr :evaluation-value s)) next-stack))
      (format t "~%~%")
      ;; HTML
      (write-piece-list-as-html
       (mapcar #'(lambda (state) (assocdr :frame state))
               next-stack))
      ;; finish or recursive
      (cond (;; solution is car
             (zero-shape-piece-p (assocdr :frame (car next-stack))) ;; todo
             (list (car next-stack)))
            (t
             (search-solution-aux
              next-stack primary-piece-list)))
       ;;
      )) ;; and report at last
  )


(defun search-solution-from-prime-pieces (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (eq '- (piece-pm-sign p))) primary-pieces)))
    (cond
      ((not (= 1 (length frame-pieces)))
       (warn (format nil "whole-piece-list has multiple frames. IDs: ~A~%"
                     (mapcar #'piece-id frame-pieces)))
       nil)
      (t 
       (let* ((frame-piece    (car frame-pieces))
              ;;
              (none-frame-pieces (remove frame-piece primary-pieces :test #'equalp))
              (stack-of-states_t0
                (list `((:frame . ,frame-piece)))))
         (search-solution-aux stack-of-states_t0 none-frame-pieces))))))

