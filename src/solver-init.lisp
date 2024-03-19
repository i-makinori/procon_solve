(in-package :puzzle-1617)

;;;; paarameter and variables and init solver functions


;;; parameters and variables

(defparameter *n-search-iter* 0) ;; variable
(defparameter *n-search-iter-max* 30) ;; parameter

(defparameter *beam-stack-width-const* 200 "max stack width for search of its beam") ;; parameter
(defparameter *fs-stackwidth-const* 100)

;;; function configs

(defparameter *step-function* #'identity)

(defparameter *evaluation-function* #'identity)


;;; init solver functions


(defun init-dictionaries ()
  ;;; dictionaries
  (setf *partial-angle-dictionary* (make-dictionary))
  (setf *partial-length^2-dictionary* (make-dictionary)))

(defun init-meta-params (primary-piece-list
                         &key (iter-max 3000) (stack-width-const-1 200) (beam-width 6))
  ;;; parameters
  ;; 
  (setf *n-search-iter* 0) ;; variable
  (setf *n-search-iter-max* iter-max) ;; parameter
  ;; stack size
  (setf *beam-stack-width-const* 150) ;; todo "max stack width for search of its beam"
  (setf *fs-stackwidth-const* stack-width-const-1) ;; todo "max stack width for common storage"
  ;; beam param
  (setf *beam-current-index* 0)
  (setf *beam-width* beam-width)

  ;;; functions
  (setf *step-function*
        ;;#'all-synthesizeable-patterns-of-pieces-to-frame
        ;;#'all-synthesizeables-of-pieces-to-piece_del-if-e-jam-edge
        ;;#'rare-synthesizeables-of-pieces-to-piece
        ;;#'rare-synthesizeables-of-pieces-to-piece-_del-if-e-jam-edge
        #'rare-synthesizeables-of-pieces-to-piece-by-partial-problem-evaluations
        ;;#'synthesizeables-by-fusuon-method-for-stuffing ;; for debug
        )   ;;"step function to get next pieces"

  (setf *evaluation-function*
        ;;#'evaluation-value-by-delta-points_sum
        ;;#'evaluation-value-by-num-used-pieces
        ;;#'evaluation-value-by-remain-edges
        ;;#'evaluation-value-by-remain-edges-in-reduce-in-k-step
        #'evaluation-value-by-remain-edges-rest-better-synthesize
        ;;#'evaluation-value-by-remain-edge-combinations
        )


  ;; partial problem parameters
  (setf *partial-width-limit*
        (max 2000
             (min 100
                  (* (num-combination-sequence 
                      1 (* 1/2 (length (sy-select-parameters-from-piece-list primary-piece-list))))
                     0.33))))
  (setf *partial-iter-limit* ;; todo: are there some better iter limit?
        ;;(ceiling (/ *partial-width-limit* 2))) 
        (* 1/1 *partial-width-limit*))
  ;;; dictionaries
  (init-dictionaries)
  )
