
(in-package #:procon)

;;;; search


;; (show-piece-list (coordinate-text->piece-list *test-piece-file8*))@
;; (show-piece-list (synthesize-piece-list-all *test-piece-n1* *test-piece1*))

(defun delta-num-of-vertices (piece)
  (- (+ (length (piece-spots (synth-piece (piece-synth-to piece))))
        (length (piece-spots (synth-piece (piece-synth-from piece)))))
     (length (piece-spots piece))))


(defun call-dfs (piece-list)

  (let* ((frame (find-if #'piece-is-frame piece-list))
         (piece-list-d (remove-if #'piece-is-frame piece-list))   ;; remove-if is duty
         ;;
         (all-list 
          (concat (mapcar #'(lambda (piece)
                              (remove-piece-shape=from-piece-list 
                               (synthesize-piece-list-all frame piece)))
                          piece-list-d)))
         )
    ;;(show-piece-list all-list)
    
    (safety-sort
     all-list
     #'(lambda (piece1 piece2)
         (> (delta-num-of-vertices piece1)
            (delta-num-of-vertices piece2))))

    (search-frame frame piece-list-d)
    ))
    
;;(search-frame frame piece-list-d)
  




(defun search-frame (frame piece-list)
  (let* ((next-piece-list1
          (concat (mapcar #'(lambda (piece)
                              (synthesize-piece-list-all frame piece))
                          piece-list)))
         (next-piece-list2
          (safety-sort
           next-piece-list1
           #'(lambda (piece1 piece2)
               (> (delta-num-of-vertices piece1)
                  (delta-num-of-vertices piece2))))
         ))
    (cond ((null next-piece-list2) nil) ;; -> nothing
          ((null (cdr next-piece-list2)) next-piece-list2) ;; -> just
          (t 
           (show-piece-list next-piece-list2)
           (find-if
            #'(lambda (piece)
                (search-frame piece (remove (synth-piece (piece-synth-to piece))
                                            piece-list
                                            :test #'equalp)))
            next-piece-list2)))))
              


;;;; lazy
(defmacro lazy (&body body)
  (let ((forced (gensym))
	(value (gensym)))
    `(let ((,forced nil)
	   (,value nil))
       (lambda ()
	 (unless ,forced
	   (setf ,value (progn ,@body))
	   (setf ,forced t))
	 ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defparameter *integers*
  (labels ((f (n)
	      (lazy-cons n (f (1+ n)))))
	  (f 1)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst)) 
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
	      (if (lazy-null lst-cur)
                  (force (lazy-mapcan fun (lazy-cdr lst)))
                (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
	    (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          x
        (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst) 
  (if (zerop n)
      (lazy-car lst)
    (lazy-nth (1- n) (lazy-cdr lst))))

