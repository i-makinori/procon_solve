
(in-package :puzzle-1617)


;;;; functions and utilities for solver composition

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



(defun whole-set-of-point-and-edge-selections-pieces-to-frame (frame-piece piece-list)
  (flatten
   (mapcar #'(lambda (p) (whole-set-of-point-and-edge-selections-piece-piece frame-piece p))
           piece-list)))

(defun all-synthesizeable-patterns-of-pieces-to-frame (frame piece-list _primes)
  _primes
  (flatten
   (remove nil
           (mapcar #'synthesize-piece-and-piece-by-selection-piece-or-fail
                   (whole-set-of-point-and-edge-selections-pieces-to-frame
                    frame piece-list)))))



;;; all synthesizeable patterns (piece as point-list to piece)

(defun all-synthesizeable-patterns-of-pieces-to-piece-point (piece1-nth_point-n piece1 piece-list)
  (flatten
   (remove nil
           (mapcar
            #'synthesize-piece-and-piece-by-selection-piece-or-fail
            (flatten (mapcar #'(lambda (piece2) (whole-set-of-point-and-edge-selections-piece-piece
                                                 piece1 piece2
                                                 :piece1-by-nth_point-only piece1-nth_point-n))
                             piece-list))))))

(defun all-synthesizeables-of-pieces-to-piece_del-if-e-jam-edge (piece1 piece-list _primes)
  "all-synthesizeable-patterns-of-pieces-to-piece_delete-if-exists-jammed-edge"
  ;; piece-list need to be unused primary pieces
  _primes
  (let ((synths-of-each-edges
          (mapcar #'(lambda (nth_point-n) (all-synthesizeable-patterns-of-pieces-to-piece-point
                                           nth_point-n piece1 piece-list))
                  (from-m-to-n-list 0 (1- (length (piece-points piece1)))))))
    ;;(format t "LenLen: ~A~%" (mapcar #'length synths-of-each-edges))
    (cond ((member nil synths-of-each-edges) ;; Exists nil?
           ;;(format t "Cut~%")
           nil)
          (t
           ;;(format t "Deepin~%")
           (flatten synths-of-each-edges)))))







;;; synthesizeable patterns filtered by partial problem


(defparameter *partial-angle-dictionary* (make-dictionary))
(defparameter *partial-length^2-dictionary* (make-dictionary))


(defun update-dictionary-by-new-piece! (piece primary-pieces angle-dictionary length^2-dictionary)
  (let ((piece-vvsy-s (partial-value-sy-param-list piece)))
    (progn
      ;; partial Angle problem
      (format t "Partial Angle    :- ")
      (mapcar
       #'(lambda (vvsy)
           (format t "*")
           (solve-partial-angle-with-memo vvsy primary-pieces angle-dictionary))
       piece-vvsy-s)
      ;; partial Length^2 problem
      (format t "~%Partial Length^2 :- ")
      (mapcar
       #'(lambda (vvsy)
           (format t "*")
           (solve-partial-length^2-with-memo vvsy primary-pieces length^2-dictionary))
       piece-vvsy-s)
      ;;
      (format t "~%"))))

;;

(defun avaiable-choices-from-dictionary (key dictionary)
  (let* ((dict-item (dictionary-key-sets key dictionary)))
    (if (null dict-item)
        `((:partials  . nil)
          (:end-state . nil))
        `((:partials  . ,(flatten (dict-item-partials dict-item)))
          (:end-state . ,(dict-item-end-state dict-item))))))

(defun synthesize-piece-and-piece-by-vvsy-vvsy-piece-or-fail (p1-piece p1-vvsy p2-piece p2-vvsy)
  (synthesize-piece-and-piece-by-selection-piece-or-fail
   (make-sy-select :p1 p1-piece :n1 (vvsy-nc p1-vvsy) :pm1 (vvsy-pm p1-vvsy)
                   :p2 p2-piece :n2 (vvsy-nc p2-vvsy) :pm2 (vvsy-pm p2-vvsy))))

(defun synthesize-piece-if-synthesizeable-pattern-vvsy-and-partial-solutions-match
    (p1-piece p1-vvsy primary-pieces p2-vvsy
     synthable-angles angle-end-state synthable-length^2s length^2-end-state)
  ;;
  angle-end-state length^2-end-state
  ;;
  (let ((p2-id-piece (find-if #'(lambda (p) (= (vvsy-id p2-vvsy) (piece-id p))) primary-pieces))
        (synthable-pattern-vvsy-p
          (and (find (vvsy-angle p2-vvsy)    synthable-angles    :test #'ser=)
               (find (vvsy-length^2 p2-vvsy) synthable-length^2s :test #'ser=))))
    (if (and synthable-pattern-vvsy-p t)
        (synthesize-piece-and-piece-by-vvsy-vvsy-piece-or-fail
         p1-piece p1-vvsy p2-id-piece p2-vvsy)
        nil)))

(defun synthesizeable-patterns-of-specific-frame-vvsy (frame-piece frame-vvsy
                                                       primary-pieces primary-vvsy-s
                                                       angle-dictionary length^2-dictionary)
  (let* (;; angle    partials
         (synthable-angles-dict
           (avaiable-choices-from-dictionary (- *pi*2* (vvsy-angle frame-vvsy)) angle-dictionary))
         (synthable-angles (assocdr :partials synthable-angles-dict))
         ;; length^2 partials
         (synthable-length^2s-dict
           (avaiable-choices-from-dictionary (vvsy-length^2 frame-vvsy) length^2-dictionary))
         (synthable-length^2s (assocdr :partials synthable-length^2s-dict)))
    ;;(format t "Ax: ~%~A~%~A~%" syntheables-from-this-angle syntheables-from-this-length^2)
    (flatten ;; flatten removes nil also
     (mapcar
      #'(lambda (p2-vvsy)
          (synthesize-piece-if-synthesizeable-pattern-vvsy-and-partial-solutions-match
           frame-piece frame-vvsy primary-pieces p2-vvsy
           synthable-angles nil synthable-length^2s nil))
      primary-vvsy-s))))

(defun rare-synthesizeables-of-pieces-to-piece (frame-piece piece-list primary-pieces)
  ;; solve partial problem
  (update-dictionary-by-new-piece! frame-piece primary-pieces
                                   *partial-angle-dictionary* *partial-length^2-dictionary*)
  ;;
  ;; synthesize which uses solution of partial problem
  (let ((frame-vvsy-s (partial-value-sy-param-list frame-piece))
        (avaiable-vvys-s (sy-select-parameters-from-piece-list piece-list)))
    (flatten ;; flatten removes nil also
     (mapcar #'(lambda (frame-vvsy_n)
                 (synthesizeable-patterns-of-specific-frame-vvsy
                  frame-piece frame-vvsy_n primary-pieces avaiable-vvys-s
                  *partial-angle-dictionary* *partial-length^2-dictionary*))
      frame-vvsy-s))))


;;; del if exists jam edge

(defun synthesizeable-patterns-of-specific-frame-vvsy-stores-end-state
    (frame-piece frame-vvsy
     primary-pieces primary-vvsy-s
     angle-dictionary length^2-dictionary)
  (let* (;; angle    partials and (todo: state)
         (synthable-angles-dict
           (avaiable-choices-from-dictionary (- *pi*2* (vvsy-angle frame-vvsy)) angle-dictionary))
         (synthable-angles (assocdr :partials synthable-angles-dict))
         (dict-angle-state (assocdr :end-state synthable-angles-dict))
         ;; length^2 partials and (todo: state)
         (synthable-length^2s-dict
           (avaiable-choices-from-dictionary (vvsy-length^2 frame-vvsy) length^2-dictionary))
         (synthable-length^2s (assocdr :partials synthable-length^2s-dict))
         (dict-length^2-state (assocdr :end-state synthable-length^2s-dict))
         ;; all synthesize patterns
         (all-synthesize-patterns
           (flatten ;; flatten removes nil also
            (mapcar
             #'(lambda (p2-vvsy)
                 (synthesize-piece-if-synthesizeable-pattern-vvsy-and-partial-solutions-match
                  frame-piece frame-vvsy primary-pieces p2-vvsy
                  synthable-angles nil synthable-length^2s nil))
             primary-vvsy-s))))
    ;;(format t "Ax: ~%~A~%~A~%" syntheables-from-this-angle syntheables-from-this-length^2)

    ;;(format t "state: ~A, ~A~%" dict-angle-state dict-length^2-state)
    ;;

    ;; todo case by :end-state
    (cond
      ((or     (eq dict-angle-state 'all-paterns)  (eq dict-length^2-state 'all-paterns))
       `((:synthesizes . ,all-synthesize-patterns) (:state . 'sy-vvsy-conver)))
      #|
      ((or     (eq dict-angle-state 'sy-vvsy-ocilla) (eq dict-length^2-state 'sy-vvsy-ocilla))
       `((:synthesizes . ,all-synthesize-patterns) (:state . 'sy-vvsy-ocilla)))
      |#
      ((and    (eq dict-angle-state 'divergence)   (eq dict-length^2-state 'divergence))
       `((:synthesizes . ,all-synthesize-patterns) (:state . 'sy-vvsy-diverg)))
      (t
       (warn "warning something wrong. storeed end-state")
       `((:synthesizes . nil) (:state . 'sy-vvsy-conver)) ;; 'failure
       ))))

(defun synthesizeable-patterns-of-specific-frame-nth-with-vvsy-remove-if-jam
    (frame-piece nth_point-n
     primary-pieces primary-vvsy-s
     angle-dictionary length^2-dictionary)
  ;;#'(lambda (nth_point-n)
  (let* ((synths-and-state-list ;; where length = 2
           (mapcar 
            #'(lambda (frame-vvsy_n_pmx)
                (synthesizeable-patterns-of-specific-frame-vvsy-stores-end-state
                 frame-piece frame-vvsy_n_pmx primary-pieces primary-vvsy-s
                 angle-dictionary length^2-dictionary))
            (partial-value-sy-param-list frame-piece ;; frame-nth-vvsys
                                         :piece-by-nth_point-only nth_point-n)))
         ;;

         ;;
         (synths_a (assocdr :synthesizes (nth 0 synths-and-state-list)))
         (synths_b (assocdr :synthesizes (nth 1 synths-and-state-list)))
         (state_a  (assocdr :state       (nth 0 synths-and-state-list)))
         (state_b  (assocdr :state       (nth 1 synths-and-state-list)))
         ;;
         (state-returns
           (cond ((member 'sy-vvsy-conver (list state_a state_b)) 'sy-vvsy-conver)
                 ;;((member 'sy-vvsy-ocilla (list state_a state_b)) 'sy-vvsy-ocilla)
                 ((member 'sy-vvsy-diverg (list state_a state_b)) 'sy-vvsy-diverg)
                 (t 'sy-vvsy-conver))))
    ;;(format t "~A, ~A~%" "fuga")
    (cond (;; filter by jummed edge
           (or (and (eq state_a 'sy-vvsy-conver) (null synths_a))
               (and (eq state_b 'sy-vvsy-conver) (null synths_b)))
           `((:synthesizes . nil)
             (:state       . 'sy-vvsy-conver)))
          (;; not filtered
           t
           `((:synthesizes . ,(append synths_a synths_b))
             (:state       . ,state-returns))))))


(defun rare-synthesizeables-of-pieces-to-piece-_del-if-e-jam-edge 
    (frame-piece piece-list primary-pieces)
  ;; todo. implement
  ;;(all-synthesizeables-of-pieces-to-piece_del-if-e-jam-edge piece piece-list primary-pieces)
  ;;

  ;;
  ;; solve partial problem
  (update-dictionary-by-new-piece! frame-piece primary-pieces
                                   *partial-angle-dictionary* *partial-length^2-dictionary*)

  ;;
  ;; synthesize which uses solution of partial problem, filters jam edge synth
  (let* ((avaiable-vvsy-s (sy-select-parameters-from-piece-list piece-list))
         ;;
         (synthes-and-state-list-of-each-edges
           (mapcar
            #'(lambda (nth)
                (synthesizeable-patterns-of-specific-frame-nth-with-vvsy-remove-if-jam
                 frame-piece nth primary-pieces avaiable-vvsy-s
                 *partial-angle-dictionary* *partial-length^2-dictionary*))
            (from-m-to-n-list 0 (1- (length (piece-points frame-piece)))))))
    ;;(print synthes-and-state-list-of-each-edges)
    (cond (;;(member nil synths-of-each-edges)
           (find-if #'(lambda (l_n)
                        ;;(format t "~A, ~A~%"
                        ;;(assocdr :state l_n)
                        ;;(length (assocdr :synthesizes l_n)))
                        (and (eq   (assocdr :state l_n) 'sy-vvsy-conver)
                             (null (assocdr :synthesizes l_n)))
                        ;;nil ; for test of cut
                        )
                    synthes-and-state-list-of-each-edges)
           nil)
          (t
           (flatten (mapcar #'(lambda (l_n)
                                (assocdr :synthesizes l_n))
                            synthes-and-state-list-of-each-edges))))))


;;;
;;; set theoretical manipulations

(defun primary-piece-p (piece-common)
  ;; piece- null?
  (eq 'leaf (piece-leaf-or-synthed piece-common)))

(defun frame-piece-of-primary-list (primary-piece-list)
  (find-if #'(lambda (p) (shape-minus-p (piece-shape p))) primary-piece-list))

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




