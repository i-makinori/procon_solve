
(in-package :puzzle-1617)

;;;; render results to HTML
;;; HTML, SVG

;; "A (no B) no iro" means "color of (B of) A.".
;; it is the Alphabet written of the Japanese reading.
;; 

(defparameter *display-scale-multiply* 14)

;;; SVG parts

(defun tml-coord (number)
  (* number *display-scale-multiply*))

(defun tml-coord-text (number)
  ;;(round (* number *display-scale-multiply*)))
  (format nil "~,4f" (tml-coord number)))

(defun point-list-into-svg-polygon (point-list pm)
  (let* ((points-text
           (apply #'concatenate 'string
                  (mapcar #'(lambda (p) (format nil "~A,~A "
                                                (tml-coord-text (vec3-x p))
                                                (tml-coord-text (vec3-y p))))
                          point-list)))
         (fill-color (cond ;; suna no iro, hinoki no iro, sora no tairakeki iro,
                       ((eq pm *+shape*) "#feecd6")
                       ((eq pm *-shape*) "#eadec9")
                       (t                "#fcf5e3")))
         ;; nil no annkoku no iro
         (style (format nil "style='fill:~A;stroke:#16160e;stroke-width:0.7071067'" fill-color)))
    (format nil "<polygon points='~A' ~A />~%"
            points-text style)))

(defun approx-coords-into-svg-dots (approx-coords-list &key (frame-piece-p nil))
  (let* ((template
           (if frame-piece-p
               ;; yuki no iro, hinoki no ha no iro, 
               "<circle r='2' cx='~A' cy='~A' fill='#eaeff3' />" ;; waku qi, frame
               "<circle r='1' cx='~A' cy='~A' fill='#648131' />" ;; hozo qi, piece (none framed)
               ))
         (text (apply #'concatenate 'string
                      (mapcar #'(lambda (p) (format nil template
                                                    (tml-coord-text (vec3-x p))
                                                    (tml-coord-text (vec3-y p))))
                              approx-coords-list))))
    text))

;;; piece list version HTML

(defun shape-svg-element-text (shape &key (frame-piece-p nil))
  (format nil
          ;;"~A~%"
          "~A~A~%"
          (point-list-into-svg-polygon (shape-coord-points shape) (shape-pm-sign shape))
          (approx-coords-into-svg-dots (shape-approx-points shape)
                                       :frame-piece-p frame-piece-p)))


(defparameter *tml-id* 0)
(defun incf-tml-id! ()
  (incf *tml-id*))


(defun piece-id-tml-string (piece)
  (format nil "Piece_~A_~A_" (piece-id piece) *tml-id*))

(defun piece-id-tml-string-svg (piece)
  (format nil "svg_~A" (piece-id-tml-string piece)))

(defun piece-id-tml-string-describe (piece)
  (format nil "describe_~A" (piece-id-tml-string piece)))

#|
(defun piece-into-svg-element-aux (piece)
  (if (null piece)
      ""
      (let ((current-text (shape-svg-element-text (piece-shape piece))))
        (cond ((equal 'leaf (piece-leaf-or-synthed piece))
               ;;current-text
               "")
              (t
               (format nil "~A~A~A"
                       current-text
                       (piece-into-svg-element-aux
                        (transform-piece (piece-transform1 piece)))
                       (piece-into-svg-element-aux
                        (transform-piece (piece-transform2 piece)))))))))
|#

(defun piece-into-svg-element-aux1 (piece transformation-matrixes-reversed)
  (cond ((null piece) "")
        ((primary-piece-p piece)
         (let ((transfomed-shape
                 ;; transform-shape-by-transformation-matrix (shape transformation-matrix)
                 (transform-shape-by-transformation-matrix
                  (piece-shape piece)
                  (reduce #'matrix3x3-product (reverse transformation-matrixes-reversed))))
               (frame?
                 (if (shape-minus-p (piece-pm-sign piece)) t nil)))
           (shape-svg-element-text transfomed-shape :frame-piece-p frame?)))
        (t
         (let ((trans1 (piece-transform1 piece))
               (trans2 (piece-transform2 piece)))
           (format nil "~A~A~A"
                   ""
                   (piece-into-svg-element-aux1
                    (transform-piece trans1)
                    (cons (transform-transformation-matrix trans1)
                          transformation-matrixes-reversed))
                   (piece-into-svg-element-aux1
                    (transform-piece trans2)
                    (cons (transform-transformation-matrix trans2)
                          transformation-matrixes-reversed))
                   )))))

(defun piece-svg-viewbox-string (piece)
  ;; viewBox="X_min Y_min X_length Y_length" (without dimentions).
  ;; <svg viewBox="0 0 200 200"> ;; , for example.
  (let* ((max-piece
           (cond ((or (shape-minus-p (piece-shape piece))
                      (zero-shape-p  (piece-shape piece)))
                  (frame-piece-of-primary-list
                   (list-of-primary-piece-list-of-synthesized-piece piece)))
                 ((shape-plus-p (piece-shape piece))
                  piece)))
         (domain-rect ((lambda (cs) (if (null cs) '(0 0 0 0) (shape-domain-rect cs)))
                       (piece-coord-points max-piece)))
         (pd ;; Piece_Domain
           (mapcar #'tml-coord domain-rect))
         (pad *display-scale-multiply*) ;; padding
         ;; viw box ranges
         (x_min (- (min 0 (domain-rect-x-min pd)) (* pad 1)))
         (y_min (- (min 0 (domain-rect-y-min pd)) (* pad 1)))
         (x_max (+ (max 0 (domain-rect-x-max pd)) (* pad 2)))
         (y_max (+ (max 0 (domain-rect-y-max pd)) (* pad 2)))
         (x_len (- x_max x_min))
         (y_len (- y_max y_min)))
    (format nil "viewbox='~A ~A ~A ~A'" x_min y_min x_len y_len)))

(defun piece-shape-coords-numbering-svg-element (piece)
  (let ((coords (piece-coord-points piece)))
    (reduce 
     #'(lambda (s1 s2) (concatenate 'string s1 s2))
     (mapcar
      #'(lambda (n)
          (format nil "<text x='~A' y='~A' font-size='6'>~A</text>~%"
                  (tml-coord (vec3-x (nth n coords)))
                  (tml-coord (vec3-y (nth n coords)))
                  n))
      (from-m-to-n-list 0 (1- (length coords))))
     :initial-value "")))

(defun piece-into-svg-element (piece)
  (let* (;; meta
         (template "<svg id='~A' ~A width='600' height='600'>~%~A~%~A~%</svg>~%")
         (viewbox (piece-svg-viewbox-string piece))
         (id-text (piece-id-tml-string-svg piece))
         ;; ume no hana moe qithesu iro.
         (origin-point-element "<circle r='5' cx='0' cy='0' fill='#f3a7a5' />~%") ;; genten
         (coord-numbers (piece-shape-coords-numbering-svg-element piece))
         (elm-memo (concatenate 'string coord-numbers origin-point-element))
         ;; elements
         ;;(elm-shape (shape-svg-element-text (piece-shape piece)))
         (tree-elements
           ;;(piece-into-svg-element-aux piece))
           (piece-into-svg-element-aux1 piece (list *identity-matrix-3x3*))))
    ;;(format nil template id-text elm-shape elm-memo )))
    (format nil template id-text viewbox
            tree-elements elm-memo)))

(defun piece-into-html-describe (piece)
  (let* ((idid_string (piece-id-tml-string-describe piece))
         (piece-text
           (format nil "piece-meta...~%id: ~A,~%(composes)[len]: ~A[~A],~%len_coord-points: ~A~%"
                   (piece-id piece)
                   (mapcar #'piece-id (list-of-primary-piece-list-of-synthesized-piece piece))
                   (length (list-of-primary-piece-list-of-synthesized-piece piece))
                   (length (piece-coord-points piece))))
         (shape-text
           (format nil "shape-info...~%shape:~A~%" (piece-shape piece))))
  (format nil
          "<pre id='~A'>~A~A</pre>"
          idid_string
          piece-text
          shape-text)))

(defun html-of-piece-list (piece-list)
  (let* (;; limitage
         (len-piece-limit-per-page 180)
         (len-limited-piece-list (subseq piece-list 0
                                         (min len-piece-limit-per-page (length piece-list))))
         ;; svg XML
         (svg-alists (mapcar #'(lambda (p)
                                 (incf-tml-id!)
                                 `(,(cons :id (piece-id-tml-string p))
                                   ,(cons :svg-text (piece-into-svg-element p))
                                   ,(cons :describe-text (piece-into-html-describe p))))
                             len-limited-piece-list))
         ;; limitage-text
         (len-limitage-text
           (if (> len-piece-limit-per-page (length piece-list))
               (format nil "<p>all pieces are shown: ~A pieces.</p>"
                       (length piece-list))
               (format nil "<p>omit because so many (synthesized) pieces: ~A of ~A pieces.</p>"
                       len-piece-limit-per-page (length piece-list)))))
    ;; format via template
    (funcall (cl-template:compile-template *html-template-text*)
             (list :svgs svg-alists
                   :len-limitage-text len-limitage-text))))

;;; state list version HTML


nil ;;


;;; I/O read write

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

