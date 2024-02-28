
(in-package :puzzle-1617)

;;;; Sketches and Notes

;;;;

;;;; piece

;;;; nomial

;;;; polynomial

;;;; vector, matrix

;;;; transforms

;;;; polynomial

;;;; profile

(defun profile-remove-congruents ()
  (let ((search1
          (sort-by-delta_points
           (all-synthesizeable-patterns-of-pieces-to-frame
            (car *example-problem-9*) (cdr *example-problem-9*)))))
    ;; call profiler
    (sb-sprof:start-profiling) 

    ;; actual calculation
    (progn
      (write-piece-list-as-html (remove-congruent-from-synthesized-piece-list search1)))

    ;; stop and report
    (sb-sprof:stop-profiling)    
    (sb-sprof:report) ))



