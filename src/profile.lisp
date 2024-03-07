(in-package :puzzle-1617)

;;;; profile

(defmacro profile-form (report-format &body forms)
  (time
   `(progn
     ;; call profiler
     (sb-sprof:start-profiling)
     
     ;; actual calculation
     
     ;;(write-piece-list-as-html (remove-congruent-from-synthesized-piece-list search1))
     ,@forms
     
     ;; stop and report
     (sb-sprof:stop-profiling)
     (sb-sprof:report ,@report-format)
     ;;
     )) ;; and report at last  
  )


(defun profile-remove-congruents ()
  (let ((search1
          (sort-by-delta_points
           (all-synthesizeable-patterns-of-pieces-to-frame
            (car *example-problem-9*) (cdr *example-problem-9*)))))
    (profile-form ()
      (write-piece-list-as-html (remove-congruent-from-synthesized-piece-list search1))
      )))


(defun profile-easy-problem ()
  (profile-form ()
    ()
    (search-solution-from-prime-pieces
     (cons (car *example-problem-9*) (cdr *example-problem-9*)))
    nil
    ))

(defun profile-grad-beam (&optional (problem-number 3))
  (profile-form ()
    (search-solution-grad-beam
     (nth problem-number *problem-list*))
    nil))


(defun profile-by-sb-profile-01 (&optional (problem-number 3))
  (sb-profile:unprofile )
  ;; register package
  (sb-profile:profile "PUZZLE-1617")
  ;; run program
  (time
   (progn 
     (search-solution-grad-beam (nth problem-number *problem-list*))
     nil))
  ;; report
  (sb-profile:report))
