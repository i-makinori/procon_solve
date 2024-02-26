
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

(defun list-of-piece-of-synthesized-piece (synthesized-piece)
  (labels
      ((aux (p_n)
         (format t "==============~%~A~%" p_n)
         (cond ((null p_n) nil)
               (t
                (append
                 ;;(list (piece-id p_n))
                 (list p_n)
                 (if (piece-transform1 p_n)
                     (aux (transform-piece (piece-transform1 p_n)))
                     nil)
                 (if (piece-transform2 p_n)
                     (aux (transform-piece (piece-transform2 p_n)))
                     nil))))))
    (aux synthesized-piece)))


(defun list-of-primary-piece-id-list-of-synthesized-piece (synthesized-piece)
  )

(defun sort-by-delta_points (synthesized-piece-list)
  
  )


;;;

#|
;;;; test
(write-piece-list-as-html
              (all-synthesizeable-patterns-of-pieces-to-frame
               (car *example-problem-9*) (cdr *example-problem-9*)))


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

