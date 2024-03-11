
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



#|
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
|#

(defun update-dictionary-by-new-piece! (piece primary-pieces)
  (let ((piece-vvsy-s (partial-value-sy-param-list piece)))
    (progn
      ;; partial Angle problem
      (format t "Partial Angle    :- ")
      (mapcar
       #'(lambda (vvsy)
           (format t "*")
           (solve-partial-angle-with-memo vvsy primary-pieces *partial-angle-dictionary*))
       piece-vvsy-s)
      ;; partial Length^2 problem
      (format t "~%Partial Length^2 :- ")
      (mapcar
       #'(lambda (vvsy)
           (format t "*")
           (solve-partial-length^2-with-memo vvsy primary-pieces *partial-length^2-dictionary*))
       piece-vvsy-s)
      ;;
      (format t "~%"))))

(defun rare-synthesizeables-of-pieces-to-piece (frame-piece piece-list primary-pieces)
  ;; todo. implement
  ;;(all-synthesizeables-of-pieces-to-piece_del-if-e-jam-edge piece piece-list primary-pieces)
  (update-dictionary-by-new-piece! frame-piece primary-pieces)
  ;;
  (let ((frame-vvsy-s (partial-value-sy-param-list frame-piece))
        (avaiable-vvys-s (sy-select-parameters-from-piece-list piece-list)))
    (flatten
     (remove
      nil
      (mapcar
       #'(lambda (p1-vvsy)
           (let* ((syntheables-from-this-angle-dict-item
                    (dictionary-key-sets (- *pi*2* (vvsy-angle p1-vvsy)) *partial-angle-dictionary*))
                  (synthable-angles
                    (flatten (dict-item-partials syntheables-from-this-angle-dict-item)))
                  (syntheables-from-this-length^2-dict-item
                    (dictionary-key-sets (vvsy-length^2 p1-vvsy) *partial-length^2-dictionary*))
                  (synthable-length^2s
                    (flatten (dict-item-partials syntheables-from-this-length^2-dict-item))))
             ;;(format t "Ax: ~%~A~%~A~%" syntheables-from-this-angle syntheables-from-this-length^2)
             (flatten
              (remove
               nil
               (cond
                 ((and t t) ;; todo case by :end-state
                  (mapcar
                   #'(lambda (p2-vvsy)
                       (let ((p2-id-piece (find-if #'(lambda (p) (= (vvsy-id p2-vvsy) (piece-id p))) primary-pieces))
                             (synthable-pattern-vvys-p
                               (and (find (vvsy-angle p2-vvsy)    synthable-angles    :test #'ser=)
                                    (find (vvsy-length^2 p2-vvsy) synthable-length^2s :test #'ser=))))
                         (if synthable-pattern-vvys-p
                             (synthesize-piece-and-piece-by-selection-piece-or-fail
                              (make-sy-select :p1 frame-piece :n1 (vvsy-nc p1-vvsy) :pm1 (vvsy-pm p1-vvsy)
                                              :p2 p2-id-piece :n2 (vvsy-nc p2-vvsy) :pm2 (vvsy-pm p2-vvsy)))
                             nil)))
                   avaiable-vvys-s)))))))
       frame-vvsy-s)))))




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




