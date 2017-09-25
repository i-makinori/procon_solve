(in-package :procon)

;;;; samples ;;;;;;;;;;;;;;;;;



;;;; sampel-json-file
(defparameter *json-file-of-pieces1* "test/piece_sample.json")

;;;; piece

#|
;; piece samples of section-piece are derived from sources in comment.
(defparameter *sample-piece-list*
  (read-primirative-pieces-list-from-json-file! *json-file-of-pieces1*))
|#

;;;; sapmle-piece
(defparameter *sample-piece1*
  `((:POINTS
     (((:X . 0) (:Y . 0)) ((:X . 26) (:Y . 0)) ((:X . 26) (:Y . 5))
      ((:X . 18) (:Y . 5)) ((:X . 18) (:Y . 17)) ((:X . 15) (:Y . 24))
      ((:X . 10) (:Y . 24)) ((:X . 4) (:Y . 20)) ((:X . 0) (:Y . 20))))
    (:SYNTH-FROM NIL) (:SYNTH-TO NIL)))

;;;; sample-json1's-piece-list
(defparameter *sample-json-1-piece-list*
  '(((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 26) (:Y . 0)) ((:X . 26) (:Y . 5))
       ((:X . 18) (:Y . 5)) ((:X . 18) (:Y . 17)) ((:X . 15) (:Y . 24))
       ((:X . 10) (:Y . 24)) ((:X . 4) (:Y . 20)) ((:X . 0) (:Y . 20))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 4) (:Y . 0)) ((:X . 10) (:Y . 4))
       ((:X . 9) (:Y . 8)) ((:X . 4) (:Y . 6)) ((:X . 0) (:Y . 6))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 4) (:Y . 0)) ((:X . 9) (:Y . 2))
       ((:X . 8) (:Y . 6)) ((:X . 6) (:Y . 16)) ((:X . 0) (:Y . 16))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 6) (:Y . 0)) ((:X . 5) (:Y . 4))
       ((:X . 4) (:Y . 12)) ((:X . 24) (:Y . 18)) ((:X . 34) (:Y . 12))
       ((:X . 38) (:Y . 9)) ((:X . 38) (:Y . 17)) ((:X . 46) (:Y . 17))
       ((:X . 46) (:Y . 22)) ((:X . 0) (:Y . 22))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 1) (:Y . 0)) ((:X . 8) (:Y . 2)) ((:X . 17) (:Y . 3))
       ((:X . 20) (:Y . 14)) ((:X . 0) (:Y . 8))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 1) (:Y . 5)) ((:X . 2) (:Y . 0)) ((:X . 7) (:Y . 2))
       ((:X . 7) (:Y . 11)) ((:X . 0) (:Y . 9))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 1) (:Y . 0)) ((:X . 10) (:Y . 0)) ((:X . 12) (:Y . 8))
       ((:X . 5) (:Y . 7)) ((:X . 0) (:Y . 5))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 2) (:Y . 0)) ((:X . 7) (:Y . 0)) ((:X . 9) (:Y . 8))
       ((:X . 0) (:Y . 8))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 7) (:Y . 1)) ((:X . 9) (:Y . 10))
       ((:X . 0) (:Y . 9))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 12)) ((:X . 3) (:Y . 5)) ((:X . 11) (:Y . 0))
       ((:X . 27) (:Y . 0)) ((:X . 38) (:Y . 7)) ((:X . 37) (:Y . 12))
       ((:X . 27) (:Y . 6)) ((:X . 11) (:Y . 6)) ((:X . 1) (:Y . 16))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 10) (:Y . 0)) ((:X . 19) (:Y . 0)) ((:X . 12) (:Y . 6))
       ((:X . 10) (:Y . 10)) ((:X . 1) (:Y . 14)) ((:X . 0) (:Y . 10))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 4)) ((:X . 9) (:Y . 0)) ((:X . 12) (:Y . 7))
       ((:X . 6) (:Y . 11)) ((:X . 2) (:Y . 12))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 5)) ((:X . 8) (:Y . 5)) ((:X . 8) (:Y . 0))
       ((:X . 32) (:Y . 0)) ((:X . 32) (:Y . 7)) ((:X . 24) (:Y . 7))
       ((:X . 24) (:Y . 12)) ((:X . 8) (:Y . 12)) ((:X . 0) (:Y . 17))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 5)) ((:X . 4) (:Y . 4)) ((:X . 10) (:Y . 0))
       ((:X . 14) (:Y . 7)) ((:X . 6) (:Y . 11)) ((:X . 2) (:Y . 14))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 5)) ((:X . 4) (:Y . 2)) ((:X . 8) (:Y . 0))
       ((:X . 13) (:Y . 10)) ((:X . 3) (:Y . 16))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 10)) ((:X . 2) (:Y . 6)) ((:X . 9) (:Y . 0))
       ((:X . 16) (:Y . 0)) ((:X . 26) (:Y . 6)) ((:X . 25) (:Y . 10))
       ((:X . 24) (:Y . 15)) ((:X . 22) (:Y . 24)) ((:X . 7) (:Y . 24))
       ((:X . 3) (:Y . 17))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 2)) ((:X . 4) (:Y . 0)) ((:X . 9) (:Y . 9))
       ((:X . 5) (:Y . 12))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 15) (:Y . 0)) ((:X . 13) (:Y . 9))
       ((:X . 20) (:Y . 11)) ((:X . 35) (:Y . 13)) ((:X . 42) (:Y . 12))
       ((:X . 52) (:Y . 9)) ((:X . 52) (:Y . 17)) ((:X . 5) (:Y . 17))
       ((:X . 5) (:Y . 9))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 7)) ((:X . 8) (:Y . 7)) ((:X . 8) (:Y . 0))
       ((:X . 31) (:Y . 0)) ((:X . 31) (:Y . 5)) ((:X . 16) (:Y . 19))
       ((:X . 11) (:Y . 19)) ((:X . 0) (:Y . 12))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 2) (:Y . 0)) ((:X . 7) (:Y . 2)) ((:X . 7) (:Y . 11))
       ((:X . 0) (:Y . 9))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 2) (:Y . 0)) ((:X . 9) (:Y . 3)) ((:X . 9) (:Y . 12))
       ((:X . 5) (:Y . 11)) ((:X . 0) (:Y . 9))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 1) (:Y . 0)) ((:X . 10) (:Y . 1)) ((:X . 12) (:Y . 9))
       ((:X . 7) (:Y . 8)) ((:X . 0) (:Y . 5))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 1) (:Y . 5)) ((:X . 2) (:Y . 0)) ((:X . 7) (:Y . 0))
       ((:X . 9) (:Y . 10)) ((:X . 0) (:Y . 9))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 12) (:Y . 3)) ((:X . 15) (:Y . 11))
       ((:X . 0) (:Y . 9))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 5) (:Y . 1)) ((:X . 8) (:Y . 11))
       ((:X . 0) (:Y . 9))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 15)) ((:X . 15) (:Y . 1)) ((:X . 34) (:Y . 0))
       ((:X . 42) (:Y . 0)) ((:X . 42) (:Y . 5)) ((:X . 23) (:Y . 5))
       ((:X . 1) (:Y . 20))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 10) (:Y . 0)) ((:X . 10) (:Y . 6))
       ((:X . 12) (:Y . 12)) ((:X . 3) (:Y . 13)) ((:X . 1) (:Y . 5))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 15)) ((:X . 22) (:Y . 0)) ((:X . 36) (:Y . 0))
       ((:X . 24) (:Y . 6)) ((:X . 16) (:Y . 15))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 1)) ((:X . 9) (:Y . 0)) ((:X . 14) (:Y . 8))
       ((:X . 10) (:Y . 9)) ((:X . 3) (:Y . 11))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 2)) ((:X . 7) (:Y . 0)) ((:X . 10) (:Y . 9))
       ((:X . 3) (:Y . 10))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 6) (:Y . 0)) ((:X . 6) (:Y . 6))
       ((:X . 0) (:Y . 6))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 6) (:Y . 0)) ((:X . 9) (:Y . 5))
       ((:X . 2) (:Y . 6))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 1)) ((:X . 7) (:Y . 0)) ((:X . 10) (:Y . 7))
       ((:X . 5) (:Y . 9))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 3)) ((:X . 4) (:Y . 2)) ((:X . 9) (:Y . 0))
       ((:X . 13) (:Y . 9)) ((:X . 3) (:Y . 12))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 27) (:Y . 0)) ((:X . 27) (:Y . 4)) ((:X . 19) (:Y . 4))
       ((:X . 0) (:Y . 5)) ((:X . 0) (:Y . 0))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 17)) ((:X . 12) (:Y . 17)) ((:X . 12) (:Y . 9))
       ((:X . 8) (:Y . 0)) ((:X . 27) (:Y . 0)) ((:X . 27) (:Y . 22))
       ((:X . 0) (:Y . 22))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 25) (:Y . 0)) ((:X . 25) (:Y . 18))
       ((:X . 6) (:Y . 18)) ((:X . 3) (:Y . 11)) ((:X . 0) (:Y . 6))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 8) (:Y . 6)) ((:X . 20) (:Y . 0)) ((:X . 25) (:Y . 0))
       ((:X . 25) (:Y . 15)) ((:X . 0) (:Y . 15))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))
    ((:POINTS
      (((:X . 0) (:Y . 0)) ((:X . 100) (:Y . 0)) ((:X . 100) (:Y . 64))
       ((:X . 73) (:Y . 64)) ((:X . 73) (:Y . 59)) ((:X . 46) (:Y . 59))
       ((:X . 46) (:Y . 64)) ((:X . 0) (:Y . 64))))
     (:SYNTH-FROM NIL) (:SYNTH-TO NIL))))
