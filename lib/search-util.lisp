(in-package :procon)

;;;; Puzzle-Theory  utilities to search

(defun piece->flatten-primirative-piece-synth (piece)
  (flatten-tree-func-test 
   #'(lambda (piec) (and (null (piece-synth-to piec))
                         (null (piece-synth-from piec))))
   #'(lambda (piec) (list (synth-piece (piece-synth-to piec))
                          (synth-piece (piece-synth-from piec))))
   piece))


(defun piece->ability-synth-list (piece)
  (mapcar
   #'(lambda (consed)
       (synth piece (car consed) (cdr consed)))
   (zip-list (list *plus* *minus*)
             (upto 0 (1- (length (piece-spots piece)))))))

(defun synth+synth-list->synthesizeable-list (synth synth-list)
  (list-of-maybe->maybe-list
   (remove *nothing* 
           (mapcar #'(lambda (synth2)
                       (synthesize-able?--also--maybe-consed-easy-piece 
                        synth synth2))
               synth-list))))

(defun synth+piece->synthesizeable-list (synth piece)
  (synth+synth-list->synthesizeable-list
   synth (piece->ability-synth-list piece)))      

(defun synthesize-piece-list-all (piece1 piece2)
  ;; (show-piece-list (synthesize-piece-list-all *test-piece1* *test-piece-n1*))
  (let* ((ziped-list (zip-list (piece->ability-synth-list piece1)
                               (piece->ability-synth-list piece2)))
         (list-maybe
          (mapcar #'(lambda (consed)
                      (maybe-synthesize-piece (car consed) (cdr consed)))
                  ziped-list))
         (piece-list-d (remove *nothing* list-maybe))
         (nil-piece->just-nil-piece
          (find-if #'(lambda (piece) (is-nil-piece piece))
                piece-list-d))
         (piece-list (if nil-piece->just-nil-piece
                         (list nil-piece->just-nil-piece)
                         piece-list-d)))
    ;;(print piece-list)
    piece-list))


(defun synthesize-piece-list-all-adjust (piece-list)
  (if (some #'(lambda (piece) (or (is-nil-piece piece) (= 0 (piece-area piece))))
            piece-list)
      (list *nil-piece*) piece-list))


(defun equivalent-piece (piece1 piece2)
  )

(defun piece-shape= (piece1 piece2)
  "if same shape piece"
  (let* ((piece2-is-frame-adjust-ed
          (if (equal (piece-is-frame piece1) (piece-is-frame piece2))
              (piece->piece-s-not-frame-piece piece2) piece2))
         (piece-list (synthesize-piece-list-all piece1 piece2-is-frame-adjust-ed)))
    (find-if #'is-nil-piece piece-list)))

