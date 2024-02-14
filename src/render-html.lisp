
(in-package :puzzle-1617)

;;;; render results to HTML
;; HTML, SVG

(defparameter *display-scale-multiply* 14)

;; SVG parts

(defun tml-coord-text (number)
  (float (* number *display-scale-multiply*)))

(defun point-list-into-svg-polygon (point-list)
  (let* ((points-text
           (apply #'concatenate 'string
                  (mapcar #'(lambda (p) (format nil "~A,~A "
                                                (tml-coord-text (vec3-x p))
                                                (tml-coord-text (vec3-y p))))
                          point-list)))
         (style "style='fill:none;stroke:#555555;stroke-width:1'"))
    (format nil "<polygon points='~A' ~A />~%"
            points-text style)))

(defun approx-coords-into-svg-dots (approx-coords-list)
  (let* ((template "<circle r='1' cx='~A' cy='~A' fill='green' />~%")
         (text (apply #'concatenate 'string
                      (mapcar #'(lambda (p) (format nil template
                                                    (tml-coord-text (vec3-x p))
                                                    (tml-coord-text (vec3-y p))))
                              approx-coords-list))))
    text))

;; overlap version HTML

(defun point-list-list-into-svg (piece-points-list &key (id-string "shape_whole"))
  "piece vector list(piece) list into svg text"
  (let* ((polygon-list (mapcar #'(lambda (piece-points)
                                   (point-list-into-svg-polygon piece-points))
                               piece-points-list))
         (polygon-list-text
           (apply #'concatenate 'string polygon-list)))
    
    (format nil "<svg id='~A' width='600' height='600'>~%~A~%</svg>~%"
            id-string polygon-list-text)))

(defun html-of-piece-list-overlap (piece-list)
  "for solven puzzle"
  (let* ((list-of-piece-points
           (mapcar #'(lambda (piece) (piece-coord-points piece))
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

;; piece list version HTML

(defun shape-svg-element-text (shape)
  (format nil "~A~%~A~%"
          (point-list-into-svg-polygon (shape-coord-points shape))
          (approx-coords-into-svg-dots (shape-approx-points shape))))


(defparameter *tml-id* 0)
(defun incf-tml-id! ()
  (incf *tml-id*))


(defun piece-id-tml-string (piece)
  (format nil "Piece_~A_~A_" (piece-id piece) *tml-id*))

    
(defun piece-into-svg-element-aux (piece)
  (if (null piece)
      ""
      (let ((current-text (shape-svg-element-text (piece-shape piece))))
        (cond ((equal 'leaf (piece-leaf-or-synthed piece))
               current-text)
              (t
               (format nil "~A~A~A"
                       current-text
                       (piece-into-svg-element
                        (transform-piece (piece-transform1 piece)))
                       (piece-into-svg-element
                        (transform-piece (piece-transform2 piece)))))))))

(defun piece-into-svg-element (piece)
  (let* (;; meta
         (template "<svg id='~A' width='600' height='600'>~%~A~%~A~%</svg>~%")
         (id-text (piece-id-tml-string piece))
         (elm-memo "<circle r='5' cx='0' cy='0' fill='red' />~%") ;; origin point
         ;; elements
         ;;(elm-shape (shape-svg-element-text (piece-shape piece)))
         (tree-elements
           (piece-into-svg-element-aux piece)))
    ;;(format nil template id-text elm-shape elm-memo )))
    (format nil template id-text
            tree-elements elm-memo)))

(defun html-of-piece-list (piece-list)
  (let* ((svg-alists (mapcar #'(lambda (p)
                                 (incf-tml-id!)
                                 `(,(cons :id (piece-id-tml-string p))
                                   ,(cons :svg-text (piece-into-svg-element p))))
                             piece-list)))
    (funcall (cl-template:compile-template *html-template-text*)
             (list :svgs svg-alists))))

;; template

(defparameter *html-template-file* (merge-pathnames "src/viewer/htmlpage_template.html.clt"
                                                    *pathname-puzzle-1617-root*))

(defparameter *html-template-text*
  (uiop:read-file-string *html-template-file*))
  
;; render to html


(defun write-piece-list-as-html (piece-list &key (file-name "piece-list.html"))
  "for piece-list"
  (let*
      ((pathname (merge-pathnames (format nil "test/results/~A" file-name) 
                                  *pathname-puzzle-1617-root*))
    ;;   (html-text (html-of-piece-list-overlap piece-list)))
       (html-text (html-of-piece-list piece-list)))
    (handler-case
        (progn (write-string-to-file pathname html-text)
               (format t "HTML file updated at : ~A ~%" pathname)
               pathname)
      (error (e) (print e) nil))))


;;;; old implement memo writtings

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

