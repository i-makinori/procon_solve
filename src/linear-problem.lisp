(in-package :puzzle-1617)


;;;; linar-problems
;;;; such as domain


;;; upper triangular matrix
;; matrix expressed in row-vector list

(defun swap-to-none-zero-aux (vec-list col stack)
  (cond ((null vec-list) nil)
        ((not (ser= (aref (car vec-list) col) 0))
         (append vec-list (reverse stack)))
        (t (swap-to-none-zero-aux
            (cdr vec-list) col (cons (car vec-list) stack)))))

(defun upper-triangular-vector-list-aux (vec-list current-col dim-each-vec)
  ;; upper-triangular-matrix
  ;; | a  b  c | => | a  b  c   |
  ;; | d  e  f | => | 0  e' f'  |
  ;; | g  h  i | => | 0  0  j'' |
  ;;
  ;;(format t "~A, ~A, ~A~%" vec-list current-col dim-each-vec)
  (cond (;; end
         (or (null vec-list) (>= current-col dim-each-vec))
         vec-list)
        (;; swap by below conversion pattern if zero at (head row, current col) is came.
         ;; . . . . * current col . . . . . . . . . . . ;;
         ;; . | _ _ _ _ _ _ _ | XXX | _ _ _ _ _ _ _ | . ;; t-1,     ; not appered at this step
         ;; * | _ _ 0 j * * * |  => | _ _ m k * * * | * ;; t+0, current row
         ;; . | _ _ m k * * * |  => | _ _ 0 j * * * | . ;; t+0
         ;; . . . . * . . . . . . . . . . . . . . . . . ;;
         ;; but if 0col{|... 0 **** |,| ... 0 **** |,| ...} came,
         ;; it's dimension will not be affected, go to next col.
         (ser= (aref (car vec-list) current-col) 0)
         (let ((swappen-list (swap-to-none-zero-aux vec-list current-col '())))
           (if swappen-list
               ;; swap. and col will be erased by below calling.
               (upper-triangular-vector-list-aux swappen-list current-col      dim-each-vec)
               ;; 0 col, go to next col.
               (upper-triangular-vector-list-aux vec-list     (1+ current-col) dim-each-vec))))
        (t
         ;; erase current col of each none head row by head row.
         ;; | a  b  c | => | a  b  c  | ;; (1)' = (1)
         ;; | d  e  f | => | 0  e' f' | ;; (2)' = (2) - d/a * (1)
         ;; | g  h  i | => | 0  g' i' | ;; (3)' = (3) - g/a * (1)
         ;; by theses conversion at step,
         ;; | (1) | ++ recursive(| (2)', (3)', (4)', ... |)
         ;; we would get the upper-triangular-matrix.
         (let* ((v1 (car vec-list))  ;; vector      means (1).
                (erased-small-matrix ;; vector-list means | (2)', (3)', (4)', ... |.
                  (mapcar
                   #'(lambda (vn_t-1)
                       (let* ((vn_t (make-array dim-each-vec :initial-element 0))
                              (coef (/ (aref vn_t-1 current-col) (aref v1 current-col))))
                         (loop for i_col from (1+ current-col) to (1- dim-each-vec)
                               ;; (n)'_i = (n)_i -                 C                 * (1)_i
                               ;;        = (n)_i - (n)_current_col / (1)_current_col * (1)_i
                               do (setf (aref vn_t i_col)
                                        (- (aref vn_t-1 i_col) (* coef (aref v1 i_col)))))
                         vn_t))
                   (cdr vec-list))))
           (cons v1
                 (upper-triangular-vector-list-aux ;; recursive(small(| (2)', (3)', (4)', ... |))
                  erased-small-matrix
                  (1+ current-col)
                  dim-each-vec))))))


(defun upper-triangular-vector-list (vec-list)
  (upper-triangular-vector-list-aux vec-list 0 (array-dimension (car vec-list) 0)))

#|
;; usage examples

> (upper-triangular-vector-list '(#(1 4 -3) #(2 -2 7)))
(#(1 4 -3) #(0 -10 13))
> (upper-triangular-vector-list '(#(1 2) #(4 -2) #(-3 7)))
(#(1 2) #(0 -10) #(0 0))
> (upper-triangular-vector-list '(#(2 3 4 1) #(1 4 2 2) #(2 1 4 3)))
(#(2 3 4 1) #(0 5/2 0 3/2) #(0 0 0 16/5))
> (upper-triangular-vector-list '(#(2 3 4 1) #(1 4 2 2) #(2 1 4 3) #(1 2 3 0)))
(#(2 3 4 1) #(0 5/2 0 3/2) #(0 0 1 -4/5) #(0 0 0 16/5))

|#
