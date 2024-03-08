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
                               ;;        (n)'_i_col  = (n)_i_col - Coefficient * (1)_i
                               ;; where  coefficient = (n)_current_col / (1)_current_col
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


;;; rank of matrix

(defun vector= (vec1 vec2 &key (test #'=))
  ;; need vec1 and vec2 to be 1Dim
  (cond ((not (= (array-dimension vec1 0) (array-dimension vec2 0)))
         nil)
        (t (every test vec1 vec2))))

(defun rank-of-vector-list-by-upper-triangular-vector-list (upper-triangular-vector-list)
  (let* ((tv-list upper-triangular-vector-list)
         (zero-vec-nd (make-array (array-dimension (car tv-list) 0) :initial-element 0)))
    (length (remove-if #'(lambda (v) 
                           ;; todo: which is better, = or ser= ?
                           (vector= v zero-vec-nd :test #'ser=))
                       tv-list))))

(defun rank-of-vector-list (vector-list)
  (rank-of-vector-list-by-upper-triangular-vector-list
   (upper-triangular-vector-list vector-list)))



;;; simultaneous domain

(defparameter *infinity-point* (vec3 100003 (exp 1) 1)) ;; (infinity, e, 1)


(defun domain-of-line-p1p2-dirs-p3 (coord-p1 coord-p2 coord-p3)
  ;; domain by line is shown us,
  ;; (C0 * x + C1 * y + C2 * 1 (<||>) 0
  ;; => (C0 C1 C2 || C3)   ;; returns coefficient vector
  ;; where C3 is (<||>) which means the  left-than(+1) or right-than(-1).
  (let* ((v (vec3-sub-xy coord-p2 coord-p1))
         (dir (vec3-sub-xy coord-p3 coord-p1))
         (C0 (- (vec3-y v)))
         (C1 (+ (vec3-x v)))
         (C2 (+ (- (* (vec3-x v) (vec3-y coord-p1)))
                (+ (* (vec3-y v) (vec3-x coord-p1)))))
         (C3 ((lambda (cross) (if (> cross 0) 1 -1)) ;; todo: round standard error
              (vec3-cross-xy v dir))))
    `#(,C0 ,C1 ,C2 ,C3) ;;(domain (make-array 4 :initial-contents #(C0 C1 C2 <+1||-1> 0)))
    ))

(defun coordinate-dirs-into-simultaneous-domain (coord-points)
  ;;
  (cond ((< (length coord-points) 3)
         '())
        (t
         (mapcar #'(lambda (cp123) ;; coord poinint 1 2 3
                     (domain-of-line-p1p2-dirs-p3 (car cp123) (cadr cp123) (cddr cp123)))
                 (make-3tuple-list coord-points)))))

#|

(defun domain-of-line-p1p2-contains-p3 (coord-p1 coord-p2 coord-p3)
  ;; domain by line is shown us,
  ;; (C0 * x + C1 * y + C2 * 1 (<||>) 0
  ;; => (C0 C1 C2 || C3)   ;; returns coefficient vector
  ;; where C3 is (<||>). it means the (>)greater-than::[+1] or(||) (<)less-than::[-1]
  (let* ((v (vec3-sub-xy coord-p2 coord-p1))
         (3x  (vec3-x coord-p3))
         (3y (vec3-y coord-p3))
         (C0 (- (vec3-y v)))
         (C1 (+ (vec3-x v)))
         (C2 (+ (- (* (vec3-x v) (vec3-y coord-p1)))
                (+ (* (vec3-y v) (vec3-x coord-p1)))))
         (C3 (cond ((not (ser= 0 C1))
                    (if (> (+ 3y (* 3x (/ C0 C1)))
                           (- (/ C2 C1)))
                        +1 -1))
                   ((not (ser= 0 C0))
                    (if (> (+ 3x (* 3y (/ C1 C0)))
                           (- (/ C2 C0)))
                        +1 -1))
                   (t 
                    (warn "zero direction at the line.")
                    0))))
    ;; todo: make-array may be farster.
    `#(,C0 ,C1 ,C2 ,C3)))
|#

#|
(defun coordinate-points-into-simultaneous-domain (coord-points contains-infinity-point-p)  
  ;;
  (cond ((< (length coord-points) 3)
         '())
        (t
         (mapcar #'(lambda (cp123) ;; coord poinint 1 2 3
                     (domain-of-line-p1p2-contains-p3 (car cp123) (cadr cp123) (cddr cp123)))
                 (make-3tuple-list coord-points)))))
  
|#


;;; simultaneous equations

