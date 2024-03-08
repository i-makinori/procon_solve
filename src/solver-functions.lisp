
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

(defun all-synthesizeable-patterns-of-pieces-to-frame (frame piece-list)
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

(defun all-synthesizeables-of-pieces-to-piece_del-if-e-jam-edge (piece1 piece-list)
  "all-synthesizeable-patterns-of-pieces-to-piece_delete-if-exists-jammed-edge"
  ;; piece-list need to be unused primary pieces
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




