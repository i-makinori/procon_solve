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
  (merge-pathnames ;; shapes (f P_from_child P_from_root)
   (pathname (format nil "test/problems/~A" file-name))
   *pathname-puzzle-1617-root*))

(defun line-text-into-vector-list (line-text-string)
  (labels ((2takes-list (list current)
             (cond ((or (null list) (null (cdr list))) (reverse current))
                   (t (2takes-list (cddr list) (cons (point (car list) (cadr list)) current))))))
    (let* ((int-list
             (mapcar #'parse-integer
                     (split-string line-text-string
                                   #'(lambda (c) (char= c #\Space))))))
      (2takes-list int-list nil)
      )))

(defun line-string-into-piece (id pm-sign line-text)
  ;;todo
  (let* ((coords (line-text-into-vector-list line-text))
         (approx-points (funcall ;;#'(lambda (c) c nil)
                         #'fill-shape-domain-by-approx-loading-points
                         ;;
                         ;;#'(lambda (c) (approx-points-list (shape-domain-rect c)))
                         coords))
         (shape (shape :pm-sign pm-sign
                       :coord-points coords
                       :approx-points approx-points)))
    (piece :leaf-or-synthed 'leaf :id id :shape shape)))


(defun load-problem-file-into-puzzle (file-name)
  (let* ((file-path (problem-file-path file-name)))
    (handler-case
        (let* (;; read file
               (line-texts
                (reverse
                 (split-string (uiop:read-file-string file-path)
                               #'(lambda (c) (char= c #\NewLine)))))
               ;; ids, pm-signs
               (ids (loop for i from 0 to (1- (length line-texts)) 
                          collect i))
               (pms (cons '- ;; (i = 0) => frame-hole
                          (loop for i from 1 to (1- (length line-texts)) ;; (i >= 1) => piece
                          collect '+))))
          ;; piece list
                (mapcar #'(lambda (id pm text) (line-string-into-piece id pm text))
                        (cdr ids) pms (cdr line-texts)))
      ;; handle error
      (sb-ext:file-does-not-exist (e) e
        (warn (format nil "problem file: '~A' is not found.~%" file-path))
        nil)
      (error (e)
        (warn (format nil "there are format errors under loaded file.~% ~A~%" e))
        nil))))
