(in-package :puzzle-1617)



;;;; I/O functions

(defun write-string-to-file (pathname string)
  (with-open-file (f pathname :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (write-sequence string f)))


;;;; I/O for puzzle solver.

(defparameter *pathname-puzzle-1617-root*
  (asdf:system-relative-pathname :puzzle-1617 nil))

(defun problem-file-path (file-name)
  (let ((own-root-pathname (merge-pathnames
                            (pathname (format nil "test/problems/~A" file-name))
                            *pathname-puzzle-1617-root*)))
    own-root-pathname))

(defun line-text-into-vector-list (line-text-string)
  (labels ((2takes-list (list current)
             (cond ((or (null list) (null (cdr list))) (reverse current))
                   (t (2takes-list (cddr list) (cons (point (car list) (cadr list)) current))))))
    (let* ((int-list
             (mapcar #'(lambda (s) (float (parse-integer s)))
                     (split-string line-text-string
                                   #'(lambda (c) (char= c #\Space))))))
      (2takes-list int-list nil)
      )))

(defun line-string-into-piece (id pm-sign line-text)
  (let* ((coords-with-side-effect (line-text-into-vector-list line-text))
         (coords-ommit-duplicates (ommit-edge-points coords-with-side-effect))
         (shape (shape pm-sign coords-ommit-duplicates)))
    (piece :leaf-or-synthed 'leaf :id id :shape shape)))

(defun load-problem-file-into-puzzle (file-name)
  (let* ((file-path (cond ((probe-file file-name)
                           file-name)
                          ((probe-file (problem-file-path file-name))
                           (problem-file-path file-name))
                          (t
                           (warn (format nil "finle-name not found ~A~%" file-name))
                           ""))))
    (handler-case
        (let* (;; read file
               (line-texts
                 (reverse
                  (split-string (uiop:read-file-string file-path)
                                #'(lambda (c) (char= c #\NewLine)))))
               (ids (from-m-to-n-list 0 (1- (length line-texts))))
               (pms ;; (i=0)=>frame, (i>=1)=>piece
                 (cons *-shape*
                       (mapcar #'(lambda (i) i *+shape*) (cdr ids)))))
          ;; piece list
          (mapcar #'(lambda (id pm text) (line-string-into-piece id pm text))
                  ids pms line-texts))
      ;; handle error
      (sb-ext:file-does-not-exist (e) e
        (warn (format nil "problem file: '~A' is not found.~%" file-path))
        nil)
      (error (e)
        (warn (format nil "there are format errors under loaded file.~% ~A~%" e))
        nil))))
