
(in-package :puzzle-1617)

;;;; solver




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
             (remove-plus-piece-overs-frame-from-synthesized-piece-list
              (remove-no-future-shaped-piece-from-synthesized-piece-list
               (remove-congruent-from-synthesized-piece-list
                (sort-by-delta_points
                 (all-synthesizeable-patterns-of-pieces-to-frame
                  piece-frame primary-piece-rests)))
               primary-piece-list)
              ;; todo. this is once old frame. frame of this step is may be better
              (assocdr :frame state)))
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
      (cond (;; no methods
             (null next-stack)
             (format t "there is no solutions. IDs: ~A~%" (mapcar #'piece-id primary-piece-list))
             nil)
            (;; car is solution, however return all
             (zero-shape-piece-p (assocdr :frame (car next-stack)))
             next-stack)
            (;; search-next
             t
             (search-solution-aux
              next-stack primary-piece-list)))
      )) ;; and report at last
  )


(defun search-solution-from-prime-pieces (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
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
                (list `((:frame . ,frame-piece))))
              (solution-and-paths (search-solution-aux stack-of-states_t0 none-frame-pieces)))
         (mapcar #'(lambda (s) (assocdr :frame s)) solution-and-paths)
)))))

