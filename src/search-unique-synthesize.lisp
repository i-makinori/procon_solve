
(in-package :puzzle-1617)

;;;; Search in unique-synthesized piece in (Breadth-First Search) method.

;; util
(defun remove-congruent-shape-from-synthesized-piece-list (synthesized-piece-list)
  ;; remove congruent shaped synthesize
  ;; if Shape(P1 + P2) = Shape(P3 + P5) , P1 + P2 remains.
  (let ((lis synthesized-piece-list))
    (cond ((null lis) '())
          (t (cons (car lis)
                   (remove-congruent-shape-from-synthesized-piece-list
                    (remove-if #'(lambda (p)
                                   (detect-piece-congruent (car lis) p t))
                               (cdr lis))))))))

;; synthesize at point


(defun maybe-unique-synthesize-to-piece-1point (nth-point piece piece-list)
  ;;
  (let* ((avaiable-vvsy-s (sy-select-parameters-from-piece-list piece-list))
         (synth-patterns-and-end-state 
           (synthesizeable-patterns-of-specific-frame-nth-with-vvsy-remove-if-jam            
             piece nth-point piece-list
             avaiable-vvsy-s *partial-angle-dictionary* *partial-length^2-dictionary*))
         (demi-unique-patterns
           (remove-congruent-shape-from-synthesized-piece-list
            (assocdr :synthesizes synth-patterns-and-end-state))))
    (cond 
      ((and
        ;; converge
        (eq 'sy-vvsy-conver (assocdr :state synth-patterns-and-end-state))
        ;; unique
        (= 1 (length demi-unique-patterns))
        ;; not jam
        (not (null
              (rare-synthesizeables-of-pieces-to-piece-_del-if-e-jam-edge 
               (car demi-unique-patterns)
               (list-of-unused-piece-list-of-piece piece piece-list)
               (list-of-unused-piece-list-of-piece piece piece-list)))))
       (car demi-unique-patterns))
      (t 
       nil))))


(defun maybe-unique-synthesize-to-piece-1step (piece piece-list primary-piece-list)
  primary-piece-list
  ;; solve partial problem
  (update-dictionary-by-new-piece! piece primary-piece-list
                                   *partial-angle-dictionary* *partial-length^2-dictionary*)
  (let* ((synthed-piece
           (reduce
            #'(lambda (piece_step nth_point-n)
                (let ((maybe-unique 
                        (maybe-unique-synthesize-to-piece-1point
                         nth_point-n piece_step
                         (list-of-unused-piece-list-of-piece piece piece-list))))
                  (if maybe-unique maybe-unique piece_step)))
                  ;;maybe-unique))
            (from-m-to-n-list 0 (1- (length (piece-coord-points piece))))
            :initial-value piece))
         )
    synthed-piece))

;;

(defun search-unique-synthesize-bfs-aux (primary-piece-list current-iter)
  (cond 
    ((null (cdr primary-piece-list)) ;; uniquely synthesized
     primary-piece-list)
    ((> current-iter (length primary-piece-list)) ;; end
     primary-piece-list)
    (t                               ;; recursive
     (let* ((piece-synth-to (car primary-piece-list))
            (piece-synth-for (cdr primary-piece-list))
            ;;
            (maybe-unique-synthesize
              (maybe-unique-synthesize-to-piece-1step
               piece-synth-to piece-synth-for primary-piece-list)))
       (write-piece-list-as-html primary-piece-list)
       (search-unique-synthesize-bfs-aux
        (append
         (list-of-unused-piece-list-of-piece
          maybe-unique-synthesize primary-piece-list)
         (list maybe-unique-synthesize))
        (if (not (= (piece-id piece-synth-to) (piece-id maybe-unique-synthesize)))
            0 (+ 1 current-iter)))))))


(defun search-unique-synthesize-bfs (whole-primary-piece-list)
  (let* ((primary-pieces (remove-if-not #'primary-piece-p whole-primary-piece-list))
         (frame-pieces   (remove-if-not #'(lambda (p) (shape-minus-p (piece-pm-sign p)))
                                        primary-pieces)))
    ;;
    (init-meta-params whole-primary-piece-list)
    ;;
    (cond
      ;; handle error
      ((not (= 1 (length frame-pieces)))
       (warn (format nil "whole-piece-list has multiple frames. IDs: ~A~%"
                     (mapcar #'piece-id frame-pieces)))
       nil)
      (t
       ;; init
       (init-meta-params primary-pieces
                         :iter-max 4000)
       ;; call-search
       (let* ((unique-synthesizes
                (search-unique-synthesize-bfs-aux whole-primary-piece-list 0)))
         unique-synthesizes)))))
