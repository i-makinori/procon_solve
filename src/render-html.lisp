
(in-package :puzzle-1617)

;;;; render results to HTML
;; HTML, SVG

(defparameter *display-scale-multiply* 14)

(defun point-list-into-svg-polygon (point-list)
  (let* ((points-text
           (apply #'concatenate 'string
                  (mapcar #'(lambda (p) (format nil "~A,~A "
                                                (* (vec3-x p) *display-scale-multiply*)
                                                (* (vec3-y p) *display-scale-multiply*)))
                          point-list)))
         (style "style='fill:none;stroke:#555555;stroke-width:1'"))
    (format nil "<polygon points='~A' ~A />~%"
            points-text style)))

(defun point-list-list-into-svg (piece-points-list &key (id-string "shape_whole"))
  "piece vector list(piece) list into svg text"
  (let* ((polygon-list (mapcar #'(lambda (piece-points)
                                   (point-list-into-svg-polygon piece-points))
                               piece-points-list))
         (polygon-list-text
           (apply #'concatenate 'string polygon-list)))
    
    (format nil "<svg id='~A' width='600' height='600'>~%~A~%</svg>~%"
            id-string polygon-list-text)))

;; template

(defparameter *html-template-file* (merge-pathnames "src/viewer/htmlpage_template.html.clt"))

(defparameter *html-template-text*
  (uiop:read-file-string *html-template-file*))
  

(defun html-of-piece-list-overlap (piece-list)
  "for solven puzzle"
  (let* ((list-of-piece-points
           (mapcar #'(lambda (piece) (shape-coord-points (piece-shape piece)))
                   piece-list))
         (id1 "overlapped_pieces")
         (overlap-alist
           `(,(cons :id id1)
             ,(cons :svg-text
                    (point-list-list-into-svg list-of-piece-points
                                              :id-string id1))))
         (svg-alists (list overlap-alist)))
  (funcall (cl-template:compile-template *html-template-text*)
           (list :svgs svg-alists))))

(defun write-piece-list-as-html (file-name piece-list)
  "for piece-list"
  (let*
      ((pathname (merge-pathnames (format nil "test/results/~A" file-name) 
                                  *pathname-puzzle-1617-root*))
       (html-text (html-of-piece-list-overlap piece-list)))
    (handler-case
        (progn (write-string-to-file pathname html-text)
               (format t "HTML file updated at : ~A ~%" pathname)
               pathname)
      (error (e) (print e) nil))))


;; render to html

#|

(defun template-text-of-solven-puzzle-html (puzzle &key (template-file-path *html-template-file*))
  "for solven puzzle"
  (with-open-file (template-file template-file-path)
    (let*
        ((template (make-string (file-length template-file)))
         (svg-alist (mapcar
                     #'(lambda (puzzle id)
                         `((:id . ,id)
                           (:svg-text .
                           ,(vec3-list-list-into-svg (identity puzzle) :id-string id))))
                     ;; (list puzzle puzzle puzzle) (list "piyo" "fuga" "hoge") ;; listing example
                     (list puzzle) (list "puzzle_solution")
                     )))
      (read-sequence template template-file)
      (funcall (cl-template:compile-template template)
               (list :svgs svg-alist)))))

(defun write-solven-puzzle-as-html (file-name puzzle)
  "for solven puzzle"
  (let*
      ((pathname (merge-pathnames (format nil "test/results/~A" file-name) 
                                  *pathname-puzzle-1617-root*))
       (html-text (template-text-of-solven-puzzle-html puzzle)))
    (handler-case
        (progn (write-string-to-file pathname html-text)
               (format t "HTML file updated at : ~A ~%" pathname)
               pathname)
      (error (e) (print e) nil))))
|#

