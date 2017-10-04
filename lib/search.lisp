
(in-package #:procon)

;;;; search


;; (show-piece-list (coordinate-text->piece-list *test-piece-file8*))@
;; (show-piece-list (synthesize-piece-list-all *test-piece-n1* *test-piece1*))



(defun call-dfs (piece-list)

  (let* ((frame (find-if #'piece-is-frame piece-list))
         (piece-list-d (remove-if #'piece-is-frame piece-list))   ;; remove-if is duty
         )

    
    (concat (mapcar #'(lambda (piece)
                        (print piece)
                        (synthesize-piece-list-all frame piece))
                    piece-list-d)
  ))))

(defun passing-one-move (frame-piece piece-list)
  (let* ((frame-diff-point 
  )








