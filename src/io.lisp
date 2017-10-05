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
       (vecs-list (mapcar #'(lambda (consed-list) 
                               (mapcar #'(lambda (consed) (vec (car consed) (cdr consed)))
                                       consed-list))
                           consed-list-list)))
    (bad-piece-list->piece-list (mapcar #'piece vecs-list))))

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
