(in-package :procon)


;;;; read file

(defun file-get-contents! (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))


;;;; coordinate txt -> piece list

(defun coordinate-text->piece-list (file-name)
  "Path -> IO -> [Piece]"
  ;; (show-piece-list (coordinate-text->piece-list *test-piece-file4*))
  (let* 
      ((file-text (file-get-contents! file-name))
       (tokens-list (parse-to-token file-text))
       (consed-list-list (mapcar #'list-to-consed-list tokens-list))
       (spots-list (mapcar #'(lambda (consed-list) 
                               (mapcar #'(lambda (consed) (spot (car consed) (cdr consed)))
                                       consed-list))
                           consed-list-list)))
    (bad-piece-list->piece-list (mapcar #'spots->piece spots-list))))

(defun list-to-consed-list (list &optional path)
  (cond ((null list) path)
        ((null (cdr list)) (error "pair of list. he he is isolated"))
        (t (list-to-consed-list (cddr list)
                                (cons (cons (car list) (cadr list))
                                      path)))))

(defun split-text (text split-char)
  "<example> text : \"ab cde fg\" split-char #\Space
>> (\"ab\" \"cde\" \"fg\")"
  (let ((pos (position split-char text)))
    (cond ((null pos) (list text))
          (t (cons (subseq text 0 pos)
                   (split-text (subseq text (1+ pos)) split-char))))))

(defun parse-to-token (text)
  (let ((split-str (mapcar #'(lambda (s) (split-text s #\Space))
                           (split-text text #\LineFeed))))
    (mapcar #'(lambda (str) 
                (mapcar #'(lambda (s) (if (string= s "") nil (read-from-string s)))
                        str))
            split-str)))


;;;; json to piece-list

(defun json-str-to-json (piece-list-json-str)
  (with-input-from-string 
      (str piece-list-json-str)
    (json:decode-json str)))

(defun json-to-primirative-piece-list (json-data)
  (let ((piece-list-json (rest (assoc :sy json-data))))
    ;;(print piece-list-json)
    (mapcar 
     #'(lambda (primirative-piece-json)
         (make-piece
          :spots (mapcar 
                  #'(lambda (json-point)
                      (spot (rest (assoc :x json-point))
                            (rest (assoc :y json-point))))
                  (rest (assoc :points primirative-piece-json)))))
     piece-list-json)))

(defun read-primirative-pieces-list-from-json-file! (filename)
  ;; (read-primirative-pieces-list-from-json-file! *json-file-of-piecess1*)
  (let* ((file-str (file-get-contents! filename))
         (json (json-str-to-json file-str))
         (piece-list (json-to-primirative-piece-list json)))
    (bad-piece-list->piece-list piece-list)))

