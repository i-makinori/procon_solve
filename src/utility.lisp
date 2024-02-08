
(in-package :puzzle-1617)


;;;; utility

(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

(defun split-string (string &optional (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
      :while end))
