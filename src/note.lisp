
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

(defmacro profile-form (&body forms)
  (time
   `(progn
     ;; call profiler
     (sb-sprof:start-profiling) 
     
     ;; actual calculation
     
     ;;(write-piece-list-as-html (remove-congruent-from-synthesized-piece-list search1))
     ,@forms
     
     ;; stop and report
     (sb-sprof:stop-profiling)    
     (sb-sprof:report)
     ;;
     )) ;; and report at last  
  )

(defun profile-remove-congruents ()
  (let ((search1
          (sort-by-delta_points
           (all-synthesizeable-patterns-of-pieces-to-frame
            (car *example-problem-9*) (cdr *example-problem-9*)))))
    (time
     (progn
       ;; call profiler
       (sb-sprof:start-profiling) 
       
       ;; actual calculation
       
       (write-piece-list-as-html (remove-congruent-from-synthesized-piece-list search1))
     
       ;; stop and report
       (sb-sprof:stop-profiling)    
       (sb-sprof:report)
       ;;
       )) ;; and report at last
    nil
    ))

(defun profile-remove-congruents ()
  (let ((search1
          (sort-by-delta_points
           (all-synthesizeable-patterns-of-pieces-to-frame
            (car *example-problem-9*) (cdr *example-problem-9*)))))
    (profile-form
      (write-piece-list-as-html (remove-congruent-from-synthesized-piece-list search1))
      )))


(defun profile-easy-problem ()
  (profile-form
    (search-solution-from-prime-pieces
     (cons (car *example-problem-9*) (cdr *example-problem-9*)))
    nil
    ))


;;;; memo

#|
(time (progn (search-solution-from-prime-pieces
                           (cons (car *example-problem-9*) (cdr *example-problem-9*)))
                          nil))

|#
