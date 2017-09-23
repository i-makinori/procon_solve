(in-package :procon)


;;;; json to piece-list

(defun file-get-contents! (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun json-str-to-json (piece-list-json-str)
  (with-input-from-string 
      (str piece-list-json-str)
    (json:decode-json str)))

(defun json-to-primirative-piece-list (json-data)
  (let ((piece-list-json (rest (assoc :sy json-data))))
    (mapcar #'(lambda (primirative-piece-json)
               (piece (rest (assoc :points primirative-piece-json)) nil nil))
            piece-list-json)))



(defun read-primirative-pieces-list-from-json-file! (filename)
  (let* ((file-str (file-get-contents! filename))
         (json (json-str-to-json file-str))
         (piece-list (json-to-primirative-piece-list json)))
    piece-list))


;;;; test / sample
(defparameter *json-file-of-pieces* "../test/piece_sample.json")
