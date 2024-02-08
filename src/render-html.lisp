
(in-package :puzzle-1617)

;;;; render results to HTML
;; HTML, SVG

(defparameter *html-template* "
<!DOCTYPE html>
<html>
  <head> </head>
  <body> ~A <hr /> ~A </body>
</html>")

(defun html-text-by-template (body-content)
  (format nil *html-template*
          "
    <h1>solved-puzzle</h1>
    <svg width=\"100\" height=\"100\" xmlns=\"http://www.w3.org/2000/svg\">
      <circle cx=\"50\" cy=\"50\" r=\"40\" stroke=\"green\" stroke-width=\"4\" fill=\"yellow\" />
      Sorry, your browser does not support inline SVG.
    </svg> "
          body-content
          ))


(defun write-solven-puzzle-as-html (file-name puzzle)
  (let*
      ((pathname (merge-pathnames (format nil "test/results/~A" file-name) 
                                  *pathname-puzzle-1617-root*))
       (svg-text (format nil "~A~%" puzzle))
       (html-text (html-text-by-template svg-text)))
    (handler-case
        (progn (write-string-to-file pathname html-text)
               (format t "HTML file updated at : ~A ~%" pathname)
               pathname)
      (error (e) (print e) nil))))


