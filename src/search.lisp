(in-package #:procon)



;;;; pieces -> synthed piece

(defun piece-to-synth-piece (piece)
  (make-synth-piece
   :piece piece))

(defun piece-list-to-synth-piece-list (piece-list)
  (mapcar #'piece-to-synth-piece piece-list))


;;;; get the most Characteristic point

(defun prioritize-point-list (piece-list)
  (safety-sort (reduce #'append (mapcar #'prioritize-point piece-list))
               #'(lambda (n m)
                   (> (cadr n) (cadr m)))))

(defun prioritize-point (piece)
  (safety-sort (cons-list-n
                (mapcar #'(lambda (deg)
                            (cons deg piece ))
                        (piece-degrees piece)))
               #'(lambda (n m)
                   (> (cadr n) (cadr m)))))



;;; passing synth-able


(defun make-condition-piece-point (piece1 point1 piece2 point2)
  "all synthesize-pattern par each point. return:piece-condition-list"
  (let* ((piece-vectors1 (piece-vectors piece1))
         (piece-vectors2 (piece-vectors piece2))
         ;; n:num, r:two-side-rotated
         (angle1-n-n-1 (vector-angle (vec-sub (rotate-nth (- point1 1) piece-vectors1)
                                              (rotate-nth point1 piece-vectors1))))
         (angle1-n-n+1 (vector-angle (vec-sub (rotate-nth (+ point1 1) piece-vectors1)
                                              (rotate-nth point1 piece-vectors1))))
         (angle2-n-n-1 (vector-angle (vec-sub (rotate-nth (- point2 1) piece-vectors2)
                                              (rotate-nth point2 piece-vectors2))))
         (angle2-n-n+1 (vector-angle (vec-sub (rotate-nth (+ point2 1) piece-vectors2)
                                              (rotate-nth point2 piece-vectors2))))
         (angle2-r-n-n-1 (- *2pi* angle2-n-n-1))
         (angle2-r-n-n+1 (- *2pi* angle2-n-n+1)))
    (append
     (mapcar #'(lambda (deg)
                 (make-piece-condition
                  :piece piece2
                  :point point2
                  :angle deg
                  :two-side t))
             (list (- angle1-n-n-1 angle2-n-n-1) (- angle1-n-n-1 angle2-n-n+1)
                   (- angle1-n-n+1 angle2-n-n-1) (- angle1-n-n+1 angle2-n-n+1)))
     (mapcar #'(lambda (deg)
                 (make-piece-condition
                  :piece piece2
                  :point point2
                  :angle deg
                  :two-side nil))
             (list (- angle1-n-n-1 angle2-r-n-n-1) (- angle1-n-n-1 angle2-r-n-n+1)
                   (- angle1-n-n+1 angle2-r-n-n-1) (- angle1-n-n+1 angle2-r-n-n+1))))))

(defun synthesizeable-conditions (condi1 piece-condi-list)
  (remove-if
   #'(lambda (condi)
       (piece-condition-collision-detection condi1 condi))
   piece-condi-list))

(defun pieces-to-synthable-conditions (host-condi piece-list)
  (let ((host-point (piece-condition-point host-condi))
        (host-piece (piece-condition-piece host-condi)))
    (let ((conditions
           (flatten (mapcar #'(lambda (piece)
                                (mapcar #'(lambda (point)
                                            (make-condition-piece-point host-piece
                                                                        host-point
                                                                        piece point))
                                        (mapcar #'car (number-pair
                                                       (piece-degrees piece)))))
                            piece-list))))
      (synthesizeable-conditions host-condi conditions))))


;;;; synth-evalution
(defun condition-evaluation (piece1-condi piece2-able-condi)
  (let* ((line-length
          (+ (length (piece-vectors (piece-condition-piece piece1-condi)))
             (length (piece-vectors (piece-condition-piece piece2-able-condi)))))
         (synthesize (let-synthesize-piece-condition piece1-condi piece2-able-condi))
         (synthed-length (if synthesize (length (piece-vectors synthesize)) 0)))
    (if synthesize
        (- line-length synthed-length)
        0)))

(defun condition-candidate (host-piece-condi condition-list)
  (mapcar #'(lambda (condi)
              (condition-evaluation host-piece-condi condi))
          condition-list))


(defun synth-piece-to-candidate (synth-piece-list)
  "search candi date"
  (let* (;; get host-point
         (piece-list-include-host (mapcar #'synth-piece-piece synth-piece-list))
         (priority-point (prioritize-point-list piece-list-include-host))
         (host-piece (cddr (car priority-point)))
         (host-no (piece-synth-piece-no host-piece synth-piece-list))
         (host-condition (make-piece-condition :piece host-piece))
         (host-candidate (list 0 host-no host-piece))
         (piece-list (remove host-piece piece-list-include-host :test #'equalp))
         ;; synthable-condition
         (synthable-condition (pieces-to-synthable-conditions host-condition piece-list))
         ;; synth-evaluation
         (candidate-coeff (condition-candidate host-condition synthable-condition))
         (condition-candi (mapcar #'list candidate-coeff synthable-condition ))
         ;; candidate
         (candidate-sort
          (take-while (stable-sort condition-candi #'(lambda (x y) (> (car x) (car y))))
                      #'(lambda (candi) (>= (car candi) 2))))
         (candi-no (mapcar #'(lambda (condi)
                               (piece-synth-piece-no (piece-condition-piece (cadr condi))
                                                     synth-piece-list))
                           candidate-sort))
         (candidate (mapcar #'(lambda (condi no)
                               (list (car condi) no (cadr condi)))
                            candidate-sort candi-no))
         )
    (values host-candidate
            candidate)))

(defun piece-synth-piece-no (piece synth-piece-list)
  (synth-piece-no
   (find-if
    #'(lambda (sy-piece)
        (equalp piece (synth-piece-piece sy-piece)))
    synth-piece-list)))


;;;; test
(defparameter *test-synth-pieces*
  (mapcar #'(lambda (pair)
              (make-synth-piece
               :piece (cdr pair)
               :no (car pair)))
          (number-pair *test-pieces*)))

(defparameter *test-synth-pieces2*
  (mapcar #'(lambda (x)
              (piece-to-synth-piece (coord-to-piece x)))
          (list *test-piece7* *test-piece8*)))








