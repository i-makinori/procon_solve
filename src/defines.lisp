
(in-package :puzzle-1617)


;; +shape, -shape
(defparameter *+shape* '*+shape*)  ;; normal (primary and synthesized) piece
(defparameter *-shape* '*-shape*) ;; hole, frame or minused-part

(defun shape-plus-p (pm-sign)
  (eq pm-sign *+shape*))

(defun shape-minus-p (pm-sign)
  (eq pm-sign *-shape*))

;; synthesize function-sign
(defparameter *S-ADD* 'S-ADD "synthesize sign in ADD")
(defparameter *S-NEG* 'S-NEG "synthesize sign in NEGative add")

;; piece

(defstruct (piece-shape (:conc-name shape-) (:constructor shape))
  (pm-sign *+shape*) ;; *+shape* or *-shape*
  (coord-points nil)
  ;; memo for detection
  (approx-points nil)        ;; piece's shape filling approx-points.
  ;; (domain poly-inequality nil)   ;; domain poly-inequality, which determines piece shape truly.  
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
  (shape (shape)) ;; nil => nil-piece, (shape ...) => actual piece

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
