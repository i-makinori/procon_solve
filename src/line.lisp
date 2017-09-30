(in-package :procon)


;;;; line, etc ;;;;;;;;;;;;;;



;;;; structure spot

(defstruct (spot (:conc-name spot-))
  x y)

(defun spot (x y)
  (make-spot :x x :y y))


;; ceonvert 
(defun cons->spot (cons)
  "cons : (x . y) form"
  (spot (car cons) (cdr cons)))

(defun spot->list (spot)
  " = (list (spot-x spot) (spot-y spot))
deal with list as spot"
  (list (spot-x spot) (spot-y spot)))

;;;; structure line

(defstruct (piece-line (:conc-name line-))
  x1 y1 x2 y2)

(defun line (x1 y1 x2 y2)
  "make-spot-line"
  (make-piece-line 
   :x1 x1 :y1 y1 :x2 x2 :y2 y2))

(defun 2spot->line (p1 p2)
  (line (spot-x p1) (spot-y p1) (spot-x p2) (spot-y p2)))

(defun line->2spot (line)
  (values (spot (line-x1 line) (line-y1 line)) (spot (line-x2 line) (line-y2 line))))



;;;; tests
(defparameter *test-spot1* (spot 10 20))
(defparameter *test-spot2* (spot 60 80))
(defparameter *test-spot3* (spot -40 10))

(defparameter *test-line1* (line 10 20 20 20))
(defparameter *test-line2* (line 20 20 20 10))
(defparameter *test-line3* (line 20 10 10 20))
(defparameter *test-line4* (line 10 20 20 40))
(defparameter *test-line5* (line 20 20 20 40))



;;;; lib
(defun cons-if-not-nil (se1 se2)
  (if se1 
      (cons se1 se2)
      se2))

(defun square (x)
  (* x x))

(defun distance (p1 p2)
  (sqrt (+ (square (- (spot-x p1) (spot-x p2)))
           (square (- (spot-y p1) (spot-y p2))))))

(defun coordinate-to-dir (start-spot end-spot)
  (atan (- (spot-x end-spot) (spot-x start-spot)) 
        (- (spot-y end-spot) (spot-y start-spot))))

(defun line-center (line)
  (spot (/ (+ (line-x1 line) (line-x2 line)) 2)
        (/ (+ (line-y1 line) (line-y2 line)) 2)))

(defun line-equal (line1 line2)
  (or (equal line1 line2)
      (equal line1 (line (line-x2 line2) (line-y2 line2) (line-x1 line2) (line-y1 line2)))))
  
(defparameter *angle-line-criteria*
  (line 0 0 0 1)
  "unit origin of degree as line-type")

(defparameter *2PI* (* PI 2))

;;;; search

(defun adjacent (spot adj-spots)
  (list spot adj-spots))

(defun find-next (spot graph-lines)
  (mapcar #'caadr
          (remove-if-not #'(lambda (x) (equalp (car x) spot)) graph-lines)))

(defun lines-to-graph-lines (lines)
  (apply #'append
         (mapcar #'(lambda (line)
                     (multiple-value-bind (p1 p2)
                         (line->2spot line)
                       (list (adjacent p1 (list p2))
                             (adjacent p2 (list p1)))))
                 lines)))

(defun angle (start-spot center-spot end-spot)
  (let ((st-angle (coordinate-to-dir start-spot center-spot))
        (ed-angle (coordinate-to-dir end-spot center-spot)))
    (- st-angle ed-angle (if (< st-angle ed-angle) (* -2 PI) 0))))

(defmacro func-angle (name func)
  `(defun ,name (before-spot center-spot next-spots)
     (if (null next-spots)
         nil
         (reduce #'(lambda (x y)
                     (if (,func (angle before-spot center-spot x)
                                (angle before-spot center-spot y))
                         x y))
                 next-spots))))
  
(func-angle min-angle <)
(func-angle max-angle >)

(defun dfs (lines now-spot ago-spot goal-spot path)
  (cond
    ((equalp now-spot goal-spot) (reverse (cons now-spot path)))
    ((null now-spot) nil)
    (t (let ((next-lines (remove-if #'(lambda (adj)
                                        (equalp (car adj) now-spot))
                                    lines )))
         (dfs next-lines
              (max-angle ago-spot now-spot (find-next now-spot lines))
              now-spot goal-spot (cons now-spot path))))))

(defun call-search+ (lines &optional (paths '()))
  (if (null lines)
      paths
      (let* ((this-line (car lines))
             (next-lines (remove this-line lines :test #'equalp))
             (consed-path (multiple-value-bind (p1 p2)
                              (search-2path (lines-to-graph-lines lines) this-line)
                            (cons p1 p2))))
        (call-search+ next-lines
                     (cons (car consed-path) (cons (cdr consed-path ) paths))))))

(defun spots-angle (path &optional (angle nil))
  (cond
    ((null angle) (spots-angle (append  (last path) path (list (car path))) 0))
    ((null (cddr path)) angle)
    (t (spots-angle (cdr path)
                     (+ angle (angle (car path) (cadr path) (caddr path)))))))

(defun call-search (lines)
  (remove-if
   #'(lambda (path)
       (or (null path)
           (> (/ (spots-angle path) (length path))
              (/ PI 3) )))
   (call-search+ lines)))
             
(defun search-2path (graph-lines line)
  (multiple-value-bind (p1 p2)
      (line->2spot line)
    (values
     (dfs graph-lines p2 p1 p1 '())
     (dfs graph-lines p1 p2 p2 '()))))
