(in-package :procon)

;;;; tree

(defun flatten-tree-func (test-end next-tree-list tree)
  "<ex> (flatten-tree-func-test #'numberp #'cdr '(1 2 (3 (4 (6 7 8 9) 5 ()))))  
 (2 7 8 9 5) "
  (cond ((null tree) nil)
        ((funcall test-end tree) tree)
        (t (flatten (mapcar
                     #'(lambda (next-tree)
                                (flatten-tree-func test-end next-tree-list next-tree))
                     (funcall next-tree-list tree))))))

(defun flatten-tree-func-test (test-end next-tree-list tree &optional (stage 0))
  "<ex> (flatten-tree-func-test #'numberp #'cdr '(1 2 (3 (4 (6 7 8 9) 5 ()))))  
 (2 7 8 9 5) "
  (show-param stage "=======================" "")
  (show-param stage "lecel" stage)
  (show-param stage "tree" tree)
  (unless (show-param stage "test" (funcall test-end tree))
    (show-param stage "next" (funcall next-tree-list tree)))
  (let ((retu
         (cond ((null tree) nil) ;; should fix
               ((funcall test-end tree)  tree)
               (t (flatten (mapcar
                            #'(lambda (next-tree)
                                (flatten-tree-func-test test-end next-tree-list next-tree
                                                        (+ stage 1)))
                            (funcall next-tree-list tree)))))))
    (show-param stage "retu" retu)))

(defun flatten-tree-list (tree)
  (cond ((null tree) nil)
        ((listp tree) (append (flatten-tree-list (car tree))
                              (flatten-tree-list (cdr tree))))
        (t (list tree))))


