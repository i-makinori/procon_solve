(in-package #:procon)


(defparameter *coord-file* "../test/coord.txt")
(defparameter *command-file* "../test/input.txt")
(defparameter *candidate-file* "../test/output.txt")


;;;; read coordinate-file

(defun list-to-piece (list)
  (labels ((pack (x-searched list searched-list)
             (cond ((null list) searched-list)
                   (x-searched (pack nil (cdr list)
                                     (cons (cons x-searched (car list)) searched-list)))
                   (t  (pack (car list) (cdr list)
                             searched-list)))))
    (let ((packed-list (reverse (pack nil list '()))))
      (coord-to-piece packed-list))))

(defun read-coord-data(&optional (file-name *coord-file*))
  "read file-name, text data to piece"
  (let ((tokens-list (remove-nil (parse-to-token (read-file file-name)))))
    (mapcar #'list-to-piece tokens-list)))


;;;; synthesize-command-file
(defun read-command (&optional (file-name *command-file*))
  file-name
  
  )

;;;; candidate-file (candidate::candidate of piece-synthesies)
(defun coord-text (coord-list)
  (reduce #'(lambda (str1 str2)
              (concatenate 'string str1 str2))
          (mapcar #'(lambda (c)
                      (format nil "~a ~a " (round (car c)) (round (cdr c))))
                  coord-list)
          ))


(defun candidate-line-text (priority piece-no piece-condition)
  (let* ((coord (piece-vectors (piece-condition-piece piece-condition)))
         (coord-text (coord-text coord))
         (two-side (piece-condition-two-side piece-condition))
         (two-side-text (if two-side "T" "F"))
         (deg (piece-condition-angle piece-condition)))
    (format nil "~a ~a ~a/ ~a ~5$~%" priority piece-no coord-text two-side-text deg)))

(defun write-candidate-file (candidate-list &optional (file-name *candidate-file*))
  "not perfect implementation"
  (let* ((text (reduce #'(lambda (s1 s2)
                          (concatenate 'string s1 s2))
                      (mapcar #'(lambda (x)
                                  (candidate-line-text 0 0 x))
                              candidate-list))))
    file-name
    (print text))
  )





;;;; util

(defun read-file (file-name)
  (with-open-file (s file-name :direction :input)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(defun write-file (file-name text)
  (with-open-file (str file-name
                     :direction :output
                     :if-does-not-exist :create)
  (format str "~A" text)))

;;; parse-text

(defun remove-nil (token-list-list)
  "remove nil-torken and nil-line"
  (remove nil 
          (mapcar #'(lambda (token-list)
                      (remove nil token-list))
                  token-list-list)))

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
                (mapcar #'(lambda (s)
                            (if (string= s "") nil
                                (read-from-string s)))
                        str))
            split-str)))


