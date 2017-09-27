
(in-package :procon)

;;;; test utils ;;;;;;;;;;;;;;;

(defun piece-list->2016-pdata (piece-list)
  (let ((coord-seq-list (mapcar #'point-list->coord-sequence
                                (mapcar #'(lambda (piece) (car-rest-assoc :points piece))
                                        piece-list))))
     (concat-string-list-list
      (mapcar #'(lambda (list) 
                  (format nil "~{~a ~}~%" list))
       coord-seq-list))))

