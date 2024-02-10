
(in-package :puzzle-1617)


;; piece

(defstruct (piece-shape (:conc-name shape-) (:constructor shape))
  (pm-sigh '+) ;; '+ or '-
  (coord-points nil)
  ;; memo for detection
  (approx-points nil)        ;; piece's shape filling approx-points.
  ;; (domain poly-inequality nil)   ;; domain poly-inequality, which determines piece shape truly.  
  )

(defstruct (transform (:conc-name transform-) (:constructor transform))
  (function-sigh '+)
  (point-from 0)
  (direction +1)
  (piece (piece))
  ;; memo
  (transformation-matrix *identity-matrix-3x3*)
  )

(defstruct (piece (:conc-name piece-) (:constructor piece))
  ;; piece-type
  (leaf-or-synthed-piece 'leaf)

  ;; shape
  (shape (shape))

  ;; for 'leaf piece
  (id nil) ;; nil => synthed, numbered => leaf
  
  ;; for 'synthed piece
  (function-sign #'+)
  (transform1 (transform))
  (transform2 (transform))
  )

