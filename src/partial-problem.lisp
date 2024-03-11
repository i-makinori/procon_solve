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

;;; parameters

(defparameter *memo-of-partial-problem*
  nil)

(defparameter *partial-width-limit*
  ;;(expt (* 49 7) 2))
  (expt (* 49 7 1/2) 2))

;;; defines and initialize



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


;;; linear combination problem until limited iter

(defun solve-partial-problem-aux (objective-value choice-value-list
                                  this-depth-queue next-depth-queue solutions
                                  current-iter)
  (cond ((and (null this-depth-queue) (null next-depth-queue)) ;; end with all pattern completed
         (values solutions 'all-paterns))
        ((and (null this-depth-queue) t) ;; next depth
         (solve-partial-problem-aux objective-value choice-value-list
                                    next-depth-queue '() solutions (+ 1 current-iter)))
        ((> (length next-depth-queue) *partial-width-limit*) ;; end with length divergence
         (values solutions 'divergence))
        (t ;; forward and next queue at this depth.
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
            (+ 1 current-iter))))))



(defun solve-partial-problem (objective-value choice-value-list)
  (let* ((t0_combination
           (mapcar #'list
                   (remove-duplicates choice-value-list :test #'ser=))) ;; todo. sort version(?)
         (t1_queue
           (remove-if-not #'(lambda (vs) (ser<= (nth 0 vs) objective-value)) t0_combination))
         (t1_solutions
           (remove-if-not #'(lambda (vs) (ser=  (nth 0 vs) objective-value)) t0_combination)))
    (multiple-value-bind (partials end-state)
        (solve-partial-problem-aux objective-value choice-value-list
                                   t1_queue '()
                                   t1_solutions 1) ;; current_depth(t1) = 1
      (values
       (remove-equally-set-from-set-list partials :test #'ser=)
       end-state))))


;;; solve partial problem

(defun solve-partial-angle (vvsy available-piece-list)
  (multiple-value-bind
        (partials end-state)
      (solve-partial-problem (- *pi*2* (vvsy-angle vvsy))
                             (flatten (mapcar #'piece-angle-list
                                              available-piece-list)))
    (values partials end-state)))

(defun solve-partial-length^2 (vvsy available-piece-list)
  (multiple-value-bind
        (partials end-state)
      (solve-partial-problem (sqrt (vvsy-length^2 vvsy))
                             (mapcar #'sqrt (flatten (mapcar #'piece-segment-length^2-list
                                                             available-piece-list))))
    (values (mapcar #'(lambda (part_n)
                        (mapcar #'(lambda (part_n_m) (expt part_n_m 2)) part_n))
                    partials)
            end-state)))


;;; memorize dictionary

(defstruct (dictionary-item (:constructor make-dict-item) (:conc-name dict-item-))
  (:objective-value -1)
  (:partials '())
  (:end-state 'not-calculated))

(defstruct (dictionary)
  (:item-storage #()) ;; array of dictionary-item
  ;;(:test #'ser=)
  )

(defun nth-of-dictionary (key-value dictionary)
  "returns values. index and existp.
 if existp returns 't, there is equally index at the (aref dictionary-storage index).
 if existp returns 'nil, there is none equally index,
 its order of key-value is is next of returned index among dictionary-storage"
  (let* ((item-storage (dictionary-item-storage dictionary))
         (len (length item-storage))
         (power-start (ceiling (log (if (zerop len) 1 len) 2))))
    (labels ((aux (n n_1ago n_2ago power-current)
               ;;(format t "~A, ~A~%" n power-current)
               (let ((power-adjust (if (<= power-current 0) 0 power-current)))
                 (cond ((= n n_2ago)
                        (values (min n n_1ago) nil))
                       ((>= n len)
                        (aux (- n (expt 2 power-adjust)) n n_1ago (- power-current 1)))
                       ((<= n -1)
                        (values -1 nil))
                       ((ser= key-value (dict-item-objective-value (aref item-storage n)))
                        (values n t))
                       ((ser< key-value (dict-item-objective-value (aref item-storage n)))
                        (aux (- n (expt 2 power-adjust)) n n_1ago (- power-current 1)))
                       ((ser> key-value (dict-item-objective-value (aref item-storage n)))
                        (aux (+ n (expt 2 power-adjust)) n n_2ago (- power-current 1)))
                       (t (warn "something wrong for dictionary index")
                          nil)))))
      (aux (expt 2 power-start) 167 173 ;; 163 and 173 is prime far to 2^n±2^(n-1)
           (- power-start 1)))))

(defun insert-to-vector-at-n (n value vector)
  (concatenate 'vector
               (subseq vector 0 n)
               (vector value)
               (subseq vector n)))

(defun append-key-value-into-dictionary (key value dictionary)
  (multiple-value-bind (dict-nth already-exist-p)
      (nth-of-dictionary key dictionary)
    (cond ((eq already-exist-p t)
           (setf (aref (dictionary-item-storage dictionary) dict-nth)
                 (union value (aref (dictionary-item-storage dictionary) dict-nth)
                        :test #'equal)))
          ((eq already-exist-p nil)
           (setf (dictionary-item-storage dictionary)
                 (insert-to-vector-at-n (+ dict-nth 1)
                                        value
                                        (dictionary-item-storage dictionary)))))))

(defun dictionary-key-sets (key dictionary)
  (multiple-value-bind (dict-nth exist-p)
      (nth-of-dictionary key dictionary)
    (cond ((null exist-p) nil)
          (t
           (aref (dictionary-item-storage dictionary) dict-nth)))))


;;; solve with memorize dictionary

;; (defparameter *partial-angle-dictionary* (make-dictionary))

(defun solve-partial-angle-with-memo (vvsy available-piece-list dictionary)
  (let* ((objective-value (- *pi*2* (vvsy-angle vvsy)))
         (item-storage (dictionary-item-storage dictionary)))
    (multiple-value-bind (index-dush existp)
        (nth-of-dictionary objective-value dictionary)
      (cond (existp
             (let ((partials  (dict-item-partials (aref item-storage index-dush)))
                   (end-state (dict-item-end-state (aref item-storage index-dush))))
               (values partials end-state)))
            (t
             (multiple-value-bind (partials end-state)
                 (solve-partial-angle vvsy available-piece-list)
               (let ((dict-item (make-dict-item :objective-value objective-value
                                                :partials partials :end-state end-state)))
                 (setf (dictionary-item-storage dictionary)
                       (insert-to-vector-at-n (+ index-dush 1) dict-item item-storage)))
               (values partials end-state)))))))


;; (defparameter *partial-length^2-dictionary* (make-dictionary))

(defun solve-partial-length^2-with-memo (vvsy available-piece-list dictionary)
  (let* ((objective-value (vvsy-length^2 vvsy))
         (item-storage (dictionary-item-storage dictionary)))
    (multiple-value-bind (index-dush existp)
        (nth-of-dictionary objective-value dictionary)
      (cond (existp
             (let ((partials  (dict-item-partials (aref item-storage index-dush)))
                   (end-state (dict-item-end-state (aref item-storage index-dush))))
               (values partials end-state)))
            (t
             (multiple-value-bind (partials end-state)
                 (solve-partial-length^2 vvsy available-piece-list)
               (let ((dict-item (make-dict-item :objective-value objective-value
                                                :partials partials :end-state end-state)))
                 (setf (dictionary-item-storage dictionary)
                       (insert-to-vector-at-n (+ index-dush 1) dict-item item-storage)))
               (values partials end-state)))))))


#|

;;;; applied usage

;;; angle all
PUZZLE-1617> (mapcar
              #'(lambda (vvsy)
                  (solve-partial-angle-with-memo
                   vvsy
                   (cdr (nth 4 *problem-list-official*)) ;; primary
                   *partial-angle-dictionary*))
              (partial-value-sy-param-list (nth 0 (nth 4 *problem-list-official*))))
PUZZLE-1617> (length (flatten (map 'list #'dict-item-partials (dictionary-item-storage *partial-angle-dictionary*))))


;;; length^2 all
PUZZLE-1617> (mapcar
              #'(lambda (vvsy)
                  (solve-partial-length^2-with-memo
                   vvsy
                   (cdr (nth 4 *problem-list-official*)) ;; primary
                   *partial-length^2-dictionary*))
              (partial-value-sy-param-list (nth 0 (nth 4 *problem-list-official*))))

PUZZLE-1617> (length (flatten (map 'list #'dict-item-partials (dictionary-item-storage *partial-length^2-dictionary*))))


;;;; example usage

;;; partial-length^2, partial-angle

PUZZLE-1617> (solve-partial-length^2
              (nth 5 (partial-value-sy-param-list (nth 0 (nth 4 *problem-list-official*))))
              (cdr (nth 4 *problem-list-official*)))
((14.56022) (7.28011 7.28011))
T
PUZZLE-1617> (solve-partial-angle
              (nth 5 (partial-value-sy-param-list (nth 0 (nth 4 *problem-list-official*))))
              (cdr (nth 4 *problem-list-official*)))
((1.3146449 1.1071486 2.1293957) (1.3734007 1.9874988 1.19029)
 (1.9264323 1.4343938 1.19029) (2.7928216 0.9730382 0.7853982))
DIVERGENCE


;;; memo versios

PUZZLE-1617> (solve-partial-length^2-with-memo
              (nth 21 (partial-value-sy-param-list (nth 0 (nth 4 *problem-list-official*))))
              (cdr (nth 4 *problem-list-official*))
              *partial-length^2-dictionary*)

;; wait and get result
PUZZLE-1617> (solve-partial-length^2-with-memo
              (nth 21 (partial-value-sy-param-list (nth 0 (nth 4 *problem-list-official*))))
              (cdr (nth 4 *problem-list-official*))
              *partial-length^2-dictionary*)
;; same result faster

;;; also, angle
PUZZLE-1617> (solve-partial-angle-with-memo 
              (nth 22 (partial-value-sy-param-list (nth 0 (nth 4 *problem-list-official*))))
              (cdr (nth 4 *problem-list-official*))
              *partial-angle-dictionary*)
((1.8925471) (1.1071486 0.7853982) (0.8884797 1.004067))
ALL-PATERNS
PUZZLE-1617> (solve-partial-angle-with-memo 
              (nth 22 (partial-value-sy-param-list (nth 0 (nth 4 *problem-list-official*))))
              (cdr (nth 4 *problem-list-official*))
              *partial-angle-dictionary*)
((1.8925471) (1.1071486 0.7853982) (0.8884797 1.004067))
ALL-PATERNS
|#
