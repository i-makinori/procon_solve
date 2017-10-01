(in-package :procon)

;;;; samples ;;;;;;;;;;;;;;;;;

;;;; sampel-json-file
;; sample json of synth and piece
;; https://github.com/nnct-jo-ken/procon2017_kyogi_server/wiki/piece-synth-in-JSON

(defparameter *json-file-of-piecess1* "test/piece_sample.json")


;;;; generate test by json-file
;; for example,
;; REPL > (piece-list->piece-list-func-form (read-primirative-pieces-list-from-json-file! *json-file-of-piecess1*))
;; (spots->piece  (list (spot 0 0) (spot 26 0) (spot 26 5) (spot 18 5) (spot 18 17) (spot 15 24) (spot 10 24) (spot 4 20) (spot 0 20) ))
;;(spots->piece  (list (spot 0 0) (spot 4 0) (spot 10 4) (spot 9 8) (spot 4 6) (spot 0 6) ))
(defun piece-list->piece-list-func-form (piece-list)
  (mapcar 
   #'(lambda (spot-list)
       (format t "(spots->piece  (list ~{(spot~{ ~A~}) ~}))~%"
               (mapcar #'(lambda (spot)
                           (list (spot-x spot) (spot-y spot)))
                       spot-list)))
   (mapcar #'piece-spots piece-list)))



;;;; test 

(defparameter *test-piece1*
  (spots->piece  (list (spot 0 0) (spot 26 0) (spot 26 5) (spot 18 5) 
                           (spot 18 17) (spot 15 24) (spot 10 24) (spot 4 20) (spot 0 20))))


(defparameter *test-piece-list1*
  (bad-piece-list->piece-list
   (list 
    (spots->piece  (list (spot 0 0) (spot 26 0) (spot 26 5) (spot 18 5) (spot 18 17) (spot 15 24) (spot 10 24) (spot 4 20) (spot 0 20) ))
    (spots->piece  (list (spot 0 0) (spot 4 0) (spot 10 4) (spot 9 8) (spot 4 6) (spot 0 6) ))
    (spots->piece  (list (spot 0 0) (spot 4 0) (spot 9 2) (spot 8 6) (spot 6 16) (spot 0 16) ))
    (spots->piece  (list (spot 0 0) (spot 6 0) (spot 5 4) (spot 4 12) (spot 24 18) (spot 34 12) (spot 38 9) (spot 38 17) (spot 46 17) (spot 46 22) (spot 0 22) ))
    (spots->piece  (list (spot 1 0) (spot 8 2) (spot 17 3) (spot 20 14) (spot 0 8) ))
    (spots->piece  (list (spot 1 5) (spot 2 0) (spot 7 2) (spot 7 11) (spot 0 9) ))
    (spots->piece  (list (spot 1 0) (spot 10 0) (spot 12 8) (spot 5 7) (spot 0 5) ))
    (spots->piece  (list (spot 2 0) (spot 7 0) (spot 9 8) (spot 0 8) ))
    (spots->piece  (list (spot 0 0) (spot 7 1) (spot 9 10) (spot 0 9) ))
    (spots->piece  (list (spot 0 12) (spot 3 5) (spot 11 0) (spot 27 0) (spot 38 7) (spot 37 12) (spot 27 6) (spot 11 6) (spot 1 16) ))
    (spots->piece  (list (spot 10 0) (spot 19 0) (spot 12 6) (spot 10 10) (spot 1 14) (spot 0 10) ))
    (spots->piece  (list (spot 0 4) (spot 9 0) (spot 12 7) (spot 6 11) (spot 2 12) ))
    (spots->piece  (list (spot 0 5) (spot 8 5) (spot 8 0) (spot 32 0) (spot 32 7) (spot 24 7) (spot 24 12) (spot 8 12) (spot 0 17) ))
    (spots->piece  (list (spot 0 5) (spot 4 4) (spot 10 0) (spot 14 7) (spot 6 11) (spot 2 14) ))
    (spots->piece  (list (spot 0 5) (spot 4 2) (spot 8 0) (spot 13 10) (spot 3 16) ))
    (spots->piece  (list (spot 0 10) (spot 2 6) (spot 9 0) (spot 16 0) (spot 26 6) (spot 25 10) (spot 24 15) (spot 22 24) (spot 7 24) (spot 3 17) ))
    (spots->piece  (list (spot 0 2) (spot 4 0) (spot 9 9) (spot 5 12) ))
    (spots->piece  (list (spot 0 0) (spot 15 0) (spot 13 9) (spot 20 11) (spot 35 13) (spot 42 12) (spot 52 9) (spot 52 17) (spot 5 17) (spot 5 9) ))
    (spots->piece  (list (spot 0 7) (spot 8 7) (spot 8 0) (spot 31 0) (spot 31 5) (spot 16 19) (spot 11 19) (spot 0 12) ))
    (spots->piece  (list (spot 2 0) (spot 7 2) (spot 7 11) (spot 0 9) ))
    (spots->piece  (list (spot 2 0) (spot 9 3) (spot 9 12) (spot 5 11) (spot 0 9) ))
    (spots->piece  (list (spot 1 0) (spot 10 1) (spot 12 9) (spot 7 8) (spot 0 5) ))
    (spots->piece  (list (spot 1 5) (spot 2 0) (spot 7 0) (spot 9 10) (spot 0 9) ))
    (spots->piece  (list (spot 0 0) (spot 12 3) (spot 15 11) (spot 0 9) ))
    (spots->piece  (list (spot 0 0) (spot 5 1) (spot 8 11) (spot 0 9) ))
    (spots->piece  (list (spot 0 15) (spot 15 1) (spot 34 0) (spot 42 0) (spot 42 5) (spot 23 5) (spot 1 20) ))
    (spots->piece  (list (spot 0 0) (spot 10 0) (spot 10 6) (spot 12 12) (spot 3 13) (spot 1 5) ))
    (spots->piece  (list (spot 0 15) (spot 22 0) (spot 36 0) (spot 24 6) (spot 16 15) ))
    (spots->piece  (list (spot 0 1) (spot 9 0) (spot 14 8) (spot 10 9) (spot 3 11) ))
    (spots->piece  (list (spot 0 2) (spot 7 0) (spot 10 9) (spot 3 10) ))
    (spots->piece  (list (spot 0 0) (spot 6 0) (spot 6 6) (spot 0 6) ))
    (spots->piece  (list (spot 0 0) (spot 6 0) (spot 9 5) (spot 2 6) ))
    (spots->piece  (list (spot 0 1) (spot 7 0) (spot 10 7) (spot 5 9) ))
    (spots->piece  (list (spot 0 3) (spot 4 2) (spot 9 0) (spot 13 9) (spot 3 12) ))
    (spots->piece  (list (spot 27 0) (spot 27 4) (spot 19 4) (spot 0 5) (spot 0 0) ))
    (spots->piece  (list (spot 0 17) (spot 12 17) (spot 12 9) (spot 8 0) (spot 27 0) (spot 27 22) (spot 0 22) ))
    (spots->piece  (list (spot 0 0) (spot 25 0) (spot 25 18) (spot 6 18) (spot 3 11) (spot 0 6) ))
    (spots->piece  (list (spot 8 6) (spot 20 0) (spot 25 0) (spot 25 15) (spot 0 15) ))
    (spots->piece  (list (spot 0 0) (spot 100 0) (spot 100 64) (spot 73 64) (spot 73 59) (spot 46 59) (spot 46 64) (spot 0 64) )))))
