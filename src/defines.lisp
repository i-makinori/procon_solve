
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
                      (make-3tuple-list ((lambda (l) (append (cdr l) (list (car l))))
                                         coords)))))
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

(defun shape (pm-sign coord-points &optional (approx-points 'by-renew))
  (let ((approx-points (if (eq approx-points 'by-renew)
                           (fill-shape-domain-by-approx-loading-points coord-points)
                           approx-points)))
    (make-piece-shape :pm-sign pm-sign
                      :coord-points coord-points
                      :approx-points approx-points
                      ;;
                      :segment-length^2-list (coords-segment-length-xy^2-list coord-points)
                      :angle-list (coords-angle-list-yin-and-yang pm-sign coord-points))))
  
