
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

(defun list-of-piece-of-synthesized-piece (synthesized-piece)
  (labels
      ((aux (p_n)
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

(defun list-of-primary-piece-list-of-synthesized-piece (synthesized-piece primary-piece-list)
  (let ((used-pieces
          (list-of-piece-of-synthesized-piece synthesized-piece)))
    (intersection ;; set theory and
     used-pieces primary-piece-list
     :test #'(lambda (p1 p2) (equal (piece-id p1) (piece-id p2))))))

(defun list-of-unused-primary-piece-list-of-synthesized-piece (synthesized-piece primary-piece-list)
  (set-difference ;; set of (original - used)
   primary-piece-list
   (list-of-primary-piece-list-of-synthesized-piece 
    synthesized-piece primary-piece-list)
   :test #'(lambda (p1 p2) (equal (piece-id p1) (piece-id p2)))))


;;; sort by delta points

(defun sort-by-delta_points (synthesized-piece-list primary-piece-list)
  (sort synthesized-piece-list
        #'(lambda (p1 p2)
            (>
             (delta_points-of-synthesize p1 primary-piece-list)
             (delta_points-of-synthesize p2 primary-piece-list)))))

(defun delta_points-of-synthesize (synthesized-piece primary-piece-list)
  (let ((n-points-of-primary
         (apply #'+ (mapcar #'(lambda (piece) (length (piece-coord-points piece)))
                     (list-of-primary-piece-list-of-synthesized-piece 
                      synthesized-piece primary-piece-list))))
        (n-points-of-synthesized-piece
          (length (piece-coord-points synthesized-piece))))
    (- n-points-of-primary n-points-of-synthesized-piece)))


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


;; piece id list for synthesized piece
(mapcar #'piece-id
        (list-of-primary-piece-list-of-synthesized-piece
         (nth 20 (all-synthesizeable-patterns-of-pieces-to-frame
                  (car *example-problem-9*) (cdr *example-problem-9*)))
         *example-problem-9*))

(mapcar #'piece-id
        (list-of-unused-primary-piece-list-of-synthesized-piece
         (nth 20 (all-synthesizeable-patterns-of-pieces-to-frame
                  (car *example-problem-9*) (cdr *example-problem-9*)))
         *example-problem-9*))



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

(defun search-solution (frame pieces)
  
  )

