
(in-package :puzzle-1617)

;;;; render results to HTML
;; HTML, SVG

(defparameter *html-template* "
<!DOCTYPE html>
<html>
  <head> </head>
  <body> ~A ~A <hr /> </body>
</html>")

(defun html-text-by-template (body-content)
  (format nil *html-template*
          "<h1>solved-puzzle</h1>"
          body-content
          ))

(defparameter *display-scale-multiply* 14)

(defun vec3-list-into-svg-polygon (vec3-list)
  (let* ((vec3-text
           ;;(reduce #'(lambda (s1 s2) (concatenate 'string s1 s2))
           (apply #'concatenate 'string
                  (mapcar #'(lambda (vec3) (format nil "~A,~A "
                                                   (* (vec3-x vec3) *display-scale-multiply*)
                                                   (* (vec3-y vec3) *display-scale-multiply*)))
                          vec3-list)))
         (style "style='fill:none;stroke:#555555;stroke-width:1'"
         ))
    (format nil "<polygon points='~A' ~A />~%"
            vec3-text style)))

(defun vec3-list-list-into-svg (piece-points-list)
  "piece vector list(piece) list into svg text"
  (let* ((polygon-list (mapcar #'(lambda (piece-points)
                                   (vec3-list-into-svg-polygon piece-points))
                               piece-points-list))
         (polygon-list-text
           (apply #'concatenate 'string polygon-list)))
    
    (format nil "<svg width='400' height='400'>~%~A~%</svg>~%" polygon-list-text)))



(defun write-solven-puzzle-as-html (file-name puzzle)
  "for solven puzzle"
  (let*
      ((pathname (merge-pathnames (format nil "test/results/~A" file-name) 
                                  *pathname-puzzle-1617-root*))
       (svg-text (identity (vec3-list-list-into-svg (identity puzzle))))
       (html-text (html-text-by-template svg-text)))
    (handler-case
        (progn (write-string-to-file pathname html-text)
               (format t "HTML file updated at : ~A ~%" pathname)
               pathname)
      (error (e) (print e) nil))))


(defun write-point-list-list-as-html (file-name puzzle)
  "for piece list"
  (let*
      ((pathname (merge-pathnames (format nil "test/results/~A" file-name) 
                                  *pathname-puzzle-1617-root*))
       (svg-text (identity (vec3-list-list-into-svg (identity puzzle))))
       (html-text (html-text-by-template svg-text)))
    (handler-case
        (progn (write-string-to-file pathname html-text)
               (format t "HTML file updated at : ~A ~%" pathname)
               pathname)
      (error (e) (print e) nil))))




