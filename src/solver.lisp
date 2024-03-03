
(in-package :puzzle-1617)

;;;; solver

;;; all synthesizeable patterns (piece and piece)

(defun all-synthesizeable-patterns-of-piece-to-piece (piece1 piece2)
  (flatten
   (remove nil
           (mapcar #'synthesize-piece-and-piece-by-selection-piece-or-fail
                   (whole-set-of-point-and-edge-selections-piece-piece 
                    piece1 piece2)))))

;;; all synthesizeable patterns (piece-list to frame)

#|
;; Note for optimization
;;  cutten duplicated selections
;; consider fill frame by pieces, selection patten is,
;; frame piece2 piece3 piece4 ...
.  0 1 1 1 ... ;; list of synthable pattern of piece to frame
.  0 0 0 0 ... ;; piece and piece is omitted
.  0 0 0 0 ... ;;
.  0 0 0 0 ... ;;
...
...
|#

(defun all-synthesizeable-patterns-of-pieces-to-frame (frame piece-list)
  (flatten
   (remove nil 
           (mapcar #'synthesize-piece-and-piece-by-selection-piece-or-fail
                   (whole-set-of-point-and-edge-selections-pieces-to-frame
                    frame piece-list)))))

(defun whole-set-of-point-and-edge-selections-pieces-to-frame (frame-piece piece-list)
  (flatten
   (mapcar #'(lambda (p) (whole-set-of-point-and-edge-selections-piece-piece frame-piece p))
           piece-list)))

  
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


;;; no-future-shape


(defun primary-params (synthesized-piece primary-piece-list)
  (labels ((afcl (most-f restriction-constant lis)
             (apply most-f (cons restriction-constant lis))))
    (let* ((minimal-angle
             (afcl #'min *num-divergent*
                   (flatten
                    (mapcar #'(lambda (p)
                                (shape-angle-list-reverse-when-hole (piece-shape p)))
                            primary-piece-list))))
           (objective-length^2
             (cond (;; if subjective is hole[-], minimal line-length of piece[+] is objective
                    (shape-minus-p (piece-shape synthesized-piece))

                    (afcl #'min *num-divergent*
                          (flatten
                           (mapcar #'(lambda (p)
                                       (shape-line-segments-length-xy^2-list (piece-shape p)))
                                   (remove-if-not #'(lambda (p) (shape-plus-p (piece-shape p)))
                                                  primary-piece-list)))))
                   (;; if subjective is piece[+], maximum line-length of hole[-] is objective
                    (shape-plus-p (piece-shape synthesized-piece))
                    (afcl #'max *num-zero*
                          (flatten
                           (mapcar #'(lambda (p)
                                       (shape-line-segments-length-xy^2-list (piece-shape p)))
                                   (remove-if-not #'(lambda (p) (shape-minus-p (piece-shape p)))
                                                  primary-piece-list)))))
                   (;; something none Real
                    t
                    (warn "Wrong Sign Piece")
                    ;; (format nil "Wrong Sign Piece: ~A~%" synthesized-piece))
                    nil))))
      `((:minimal-angle . ,minimal-angle)
        (:objective-length^2 . ,objective-length^2))
      )))

(defun no-future-angles-p (angle-list minimal-angle)
  ;; Cut if ∃(angle_n + min_angle > 360°)
  (find-if #'(lambda (a_n)   (ser> (+ a_n minimal-angle) *pi*2*)) angle-list))

(defun no-future-hole-lines-p (hole-length^2-list minimal-length^2)
  ;; Cut if ∃(hole_length_n < min_length)
  (find-if #'(lambda (l^2_n) (ser< l^2_n minimal-length^2)) hole-length^2-list))

(defun no-future-piece-lines-p (piece-length^2-list hole-maximum-length^2)
  ;; Cut if ∃(piece_length_n > hole_max_length)
  (find-if #'(lambda (l^2_n) (ser> l^2_n hole-maximum-length^2)) piece-length^2-list))

(defun no-future-shape-p (shape primary-params)
  (let ((minimal-angle  (assocdr :minimal-angle   primary-params))
        (objective-length^2 (assocdr :objective-length^2 primary-params))
        ;;
        (angle-list (shape-angle-list-reverse-when-hole shape))
        (length^2-list (shape-line-segments-length-xy^2-list shape)))
    (or
     ;; angle
     (no-future-angles-p angle-list minimal-angle)
     ;; line length
     (cond ((shape-minus-p shape) ;; if subjective is hole [-] ,
            (no-future-hole-lines-p  length^2-list objective-length^2))
           ((shape-plus-p shape)  ;; if subjective is piece[+] ,
            (no-future-piece-lines-p length^2-list objective-length^2) )
           (t                     ;; if none Real sign
            t)))))


(defun detect-no-future-piece (synthesized-piece primary-piece-list)
  (if (zero-shape-piece-p synthesized-piece)
      nil
      (let ((primary-params (primary-params
                             synthesized-piece
                             (list-of-unused-primary-piece-list-of-synthesized-piece
                              synthesized-piece primary-piece-list))))
        (no-future-shape-p (piece-shape synthesized-piece)
                           primary-params))))

;; filter

(defun remove-no-future-shaped-piece-from-synthesized-piece-list
    (synthesized-piece-list primary-piece-list)
  (remove-if #'(lambda (synthed-piece)
                 (detect-no-future-piece synthed-piece primary-piece-list))
             synthesized-piece-list))


(defun remove-congruent-from-synthesized-piece-list (synthesized-piece-list)
  (let ((lis synthesized-piece-list))
    (cond ((null lis) '())
          (t (cons (car lis)
                   (remove-congruent-from-synthesized-piece-list
                    (remove-if #'(lambda (p)
                                   (detect-piece-congruent (car lis) p))
                               (cdr lis))))))))


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
             (remove-no-future-shaped-piece-from-synthesized-piece-list
              (remove-congruent-from-synthesized-piece-list
               (sort-by-delta_points
                (all-synthesizeable-patterns-of-pieces-to-frame
                 piece-frame primary-piece-rests)))
               primary-piece-list))
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

