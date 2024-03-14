
(in-package :puzzle-1617)

;;;; Sketches and Notes

;;;;

;;;; piece

;;;; nomial

;;;; polynomial

;;;; vector, matrix

;;;; transforms

;;;; polynomial



;;;; memo

#|

(ql:quickload :puzzle-1617)
(in-package :puzzle-1617)
(time (progn (search-solution-grad-beam (nth 4 *problem-list-official*)) nil))

|#


#|

;;;; some tests

(time (progn (search-solution-from-prime-pieces
                           (cons (car *example-problem-9*) (cdr *example-problem-9*)))
                          nil))

(write-piece-list-as-html
              (all-synthesizeable-patterns-of-pieces-to-frame
               (car *example-problem-9*) (cdr *example-problem-9*)))


(write-piece-list-as-html
 (sort-by-delta_points
  (all-synthesizeable-patterns-of-pieces-to-frame
   (car *example-problem-9*) (cdr *example-problem-9*))))
|#

#|
;;;; congruent check

> (setq *search1*
              (sort-by-delta_points
               (all-synthesizeable-patterns-of-pieces-to-frame
                (car *example-problem-9*) (cdr *example-problem-9*))))
nil
> (detect-piece-congruent (nth 1 *search1*) (nth 0 *search1*))

|#
