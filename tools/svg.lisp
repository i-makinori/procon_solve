(in-package #:procon)

;;;; io

(defparameter *html-file* "../test/ui.html")

(defun write-svg-file (text &optional (file-name *html-file*))
  (write-file file-name text))


;;; svg-lib
(defun print-tag (name alist closingp)
  (concatenate
   'string
   "<"
   (when closingp
     "/")
   (format nil "~a" (string-downcase name))
   (reduce #'(lambda (s1 s2) (concatenate 'string s1 s2))
           (cons "" (mapcar
                     #'(lambda (att)
                         (format nil " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
                            alist)))
   ">"))

(defmacro tag (name atts &body body)
  `(concatenate
    'string
    (print-tag ',name
               (list ,@(mapcar (lambda (x)
                                 `(cons ',(car x) ,(cdr x)))
                               (pairs atts)))
               nil)
    ,@body
    (print-tag ',name nil t)))

(defmacro svg (&body body)
  `(tag svg (width 400 height 200)
     ,@body))

(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

(defun circle (center radius color)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color))))

(defun polygon (points color)
  (tag polygon (points (format nil
                               "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color))))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))


;;;; html

(defmacro html-header (&body body)
  `(tag html ()
     (tag body ()
       ,@body)))

(defun piece-condi-svg (piece-condi color)
  (let* ((piece (refrect-piece-condi piece-condi))
         (point-list (mapcar #'(lambda (x) (cons (round (car x)) (round (cdr x))))
                             (append (last (piece-vectors piece)) (piece-vectors piece)))
         ))
    (polygon point-list color)
    ))

(defun axes-html ()
  (concatenate
   'string
   (circle '(0 . 0) 20 '(255 0 0))
   (polygon '((0 . 0) (100 . 100)) '(0 0 0))
  ))

(defun condition-svg-html (condi1 condi2 num)
    (concatenate
     'string
     (format nil "~%~A" num)
     (tag svg (width 400 height 200)
       (tag g (transform (format nil "translate(200, 100) scale(~a)" 0.05))
         (piece-condi-svg condi1 '(128 128 255))
         (piece-condi-svg condi2 '(128 128 128))
         (axes-html)
         )
       )
     "<hr />"
     ))

(defun write-svg (host-condi candi-number-pair)
  (let*
      ((condition-svg-html
        (string-list-to-string
         (mapcar #'(lambda (condi-pair)
                     (condition-svg-html host-condi (caddr condi-pair) (car condi-pair)))
                 (number-pair candi-number-pair))))
       (html-text (html-header () condition-svg-html)))
    (write-svg-file  html-text)
  ))



