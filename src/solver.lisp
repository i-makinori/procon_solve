
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


;;;

#|
(defun make-puzzle-statement (piece-list)
  ;; alist as puzzle statement.
  (let ((id-list (mapcar #'piece-id piece-list)))
    (list (cons 'id-list id-list)
          (cons 'frame   (find-if #'(lambda (piece) (eql -1 (piece-pm-sign piece))) id-list))
          )))
|#

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

(defun state (current-frame remains-pieces points-length)
  `((:current-frame . ,current-frame)
    (:remains . ,remains-pieces)
    (:points-length . ,points-length)))

(defun state-remains (state)
  (cdr (assoc :remains state))
  )

(defun search-solution-aux (state-stack)
  (let* ((current-state (car state-stack))
         (stack (cdr state-stack))) ;; older sorted stack
    stack
    (cond ((null (state-remains current-state)) ;; remain-piece is nil
           current-state) ;; it is goal
          )))

#|
;; usage
(search-solution
  (list `((:frame . ,(car *example-problem-9*))))
  (cdr *example-problem-9*))
|#

(defun search-solution (stack-of-states primary-piece-list)
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
            
    (let* ((patterns-of-step!
             (remove-congruent-from-synthesized-piece-list
              (sort-by-delta_points
               (all-synthesizeable-patterns-of-pieces-to-frame
                piece-frame primary-piece-rests))))
           (next-stack ;; todo sort by score of statement
             (sort
              (append (mapcar #'(lambda (next-frame)
                                  `((:frame . ,next-frame)))
                              patterns-of-step!)
                      stacking)
              #'(lambda (state1 state2)
                  (> (delta_points-of-synthesize (assocdr :frame state1))
                     (delta_points-of-synthesize (assocdr :frame state2)))))))
      ;;
      (write-piece-list-as-html
       (mapcar #'(lambda (state) (assocdr :frame state))
               next-stack))
      ;;
      (cond (;; solution is car
             (zero-shape-piece-p (assocdr :frame (car next-stack))) ;; todo
             (list (car next-stack)))
            (t
             (search-solution
              next-stack primary-piece-list)))
       ;;
      )) ;; and report at last
  )

