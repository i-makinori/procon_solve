
(in-package :puzzle-1617)


;; +shape, -shape
(defparameter *+shape* '*+shape*)  ;; normal (primary and synthesized) piece
(defparameter *-shape* '*-shape*) ;; hole, frame or minused-part

(defun shape-plus-p (pm-sign)
  ;; Todo split function by type
  (if (typep pm-sign 'piece-shape)
      (eq (shape-pm-sign pm-sign) *+shape*)
      (eq pm-sign *+shape*)))

(defun shape-minus-p (pm-sign)
  ;; Todo split function by type
  (if (typep pm-sign 'piece-shape)
      (eq (shape-pm-sign pm-sign) *-shape*)
      (eq pm-sign *-shape*)))

;; synthesize function-sign
(defparameter *S-ADD* 'S-ADD "synthesize sign in ADD")
(defparameter *S-NEG* 'S-NEG "synthesize sign in NEGative add")

;; piece

(defstruct (piece-shape (:constructor make-piece-shape) (:conc-name shape-))
  (pm-sign *+shape*) ;; *+shape* or *-shape*
  (coord-points nil)
  ;; memo for detection
  (approx-points nil)        ;; piece's shape filling approx-points.
  ;; (domain poly-inequality nil)   ;; domain poly-inequality, which determines piece shape truly.
  ;;
  (segment-length^2-list nil)
  (angle-list nil)
  )

(defstruct (transform (:conc-name transform-) (:constructor transform))
  (function-sign *s-add*)
  (point-from 0)
  (direction +1)
  (piece (piece))
  ;; memo
  (transformation-matrix *identity-matrix-3x3*)
  )

(defstruct (piece (:conc-name piece-) (:constructor piece))
  ;; piece-type
  (leaf-or-synthed 'leaf)

  ;; shape
  (shape (make-piece-shape)) ;; nil => nil-piece, (shape ...) => actual piece

  ;; for 'leaf piece
  (id nil) ;; nil => synthed, numbered => leaf

  ;; for 'synthed piece
  (function-sign nil) ;; nil => not synthed, '+ => synthe piece, '- => negative synth piece.
  (transform1 nil)
  (transform2 nil)
  )


(defun piece-pm-sign (piece)
  (shape-pm-sign (piece-shape piece)))

(defun piece-coord-points (piece)
  (shape-coord-points (piece-shape piece)))
(defun piece-points (piece)
  (piece-coord-points piece))

(defun piece-approx-points (piece)
  (shape-approx-points (piece-shape piece)))

(defun piece-angle-list (piece)
  (shape-angle-list (piece-shape piece)))

(defun piece-segment-length^2-list (piece)
  (shape-segment-length^2-list (piece-shape piece)))


;; zero element

(defun zero-shape-p (shape)
  ;; synthesized zero shape is one of the solution
  (eq nil (shape-coord-points shape)))

(defun zero-shape-piece-p (piece)
  (zero-shape-p (piece-shape piece)))


;;; shape

(defun coords-angle-list-yang (coords)
  (if (> 3 (length coords))
      nil
      (let ((n-points (length coords))
            (angle-list
              (mapcar #'(lambda (c_pcn) ;; Coordinate_Previous, Current, Next
                          (;; positive angle. (avoid negative range). from -pi to pi into 0 to 2 pi .
                           (lambda (a) (mod a *pi*2*))
                           (angle (cadr c_pcn) (car c_pcn) (cddr c_pcn))))
                      ;; 3tuple-list, rot-right 1 .
                      (make-3tuple-list (rot-right 1 coords)))))
        (if (ser= (* (- n-points 2) *pi*) ;; sum of interior angle = (N-2) * 180°
                  (reduce #'+ angle-list))
            (identity angle-list)
            (mapcar #'(lambda (angle) (- *pi*2* angle)) angle-list)))))

(defun coords-angle-list-yin-and-yang (pm-sign coords)
  (let ((angle-list1 (coords-angle-list-yang coords)))
    (cond ((eq pm-sign *-shape*)
           (mapcar #'(lambda (a) (- *pi*2* a))
                   angle-list1))
          (t
           (identity angle-list1)))))

(defun coords-segment-length-xy^2-list (coords)
  (if (> 3 (length coords))
      nil
      (mapcar #'(lambda (c_cn) ;; Coordinate_Current, Next
                  (vec3-length-xy^2 (vec3-sub-xy (cdr c_cn) (car c_cn))))
              (make-tuple-list coords))))

(defun shape (pm-sign coord-points &optional (approx-points 'by-renew) (need-memo-list 't))
  (let ((approx-points
          (if (eq approx-points 'by-renew)
              (fill-shape-domain-by-approx-loading-points coord-points)
              approx-points))
        (segment-length^2-list 
          (if need-memo-list
              (coords-segment-length-xy^2-list coord-points)
              nil))
        (angle-list
          (if need-memo-list
              (coords-angle-list-yin-and-yang pm-sign coord-points)
              nil)))
    (make-piece-shape :pm-sign pm-sign
                      :coord-points coord-points
                      :approx-points approx-points
                      ;;
                      :segment-length^2-list segment-length^2-list
                      :angle-list angle-list)))
  



;;;
;;; set theoretical manipulations

(defun primary-piece-p (piece-common)
  ;; piece- null?
  (eq 'leaf (piece-leaf-or-synthed piece-common)))

(defun frame-piece-of-primary-list (primary-piece-list)
  (find-if #'(lambda (p) (shape-minus-p (piece-shape p))) primary-piece-list))

(defun list-of-primary-piece-list-of-synthesized-piece (synthesized-piece)
  (labels ((aux (p_n)
             (cond ((primary-piece-p p_n) (list p_n))
                   (t (append
                       (if (piece-transform1 p_n)
                           (aux (transform-piece (piece-transform1 p_n)))
                           nil)
                       (if (piece-transform2 p_n)
                           (aux (transform-piece (piece-transform2 p_n)))
                           nil))))))
    (aux synthesized-piece)))

(defun list-of-piece-of-synthesized-piece (synthesized-piece)
  (labels ((aux (p_n)
             (cond ((null p_n) nil)
                   (t (append
                       (list p_n)
                       (if (piece-transform1 p_n)
                           (aux (transform-piece (piece-transform1 p_n)))
                           nil)
                       (if (piece-transform2 p_n)
                           (aux (transform-piece (piece-transform2 p_n)))
                           nil))))))
    (aux synthesized-piece)))

(defun list-of-unused-primary-piece-list-of-synthesized-piece (synthesized-piece primary-piece-list)
  (set-difference ;; set of (original - used)
   primary-piece-list
   (list-of-primary-piece-list-of-synthesized-piece synthesized-piece)
   :test #'(lambda (p1 p2) (equal (piece-id p1) (piece-id p2)))))

(defun list-of-unused-piece-list-of-piece (piece piece-list)
  (set-difference ;; set of (original - used)
   piece-list
   (list-of-piece-of-synthesized-piece piece)
   :test #'(lambda (p1 p2) (equal (piece-id p1) (piece-id p2)))))

