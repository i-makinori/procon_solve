(in-package #:procon)

(declaim (inline line point x1 y1 x2 y2 square))

(defun line (x1 y1 x2 y2)
  (list x1 y1 x2 y2))

(defun point (x1 y1)
  (list x1 y1))

(defun x1 (line)
  (car line))

(defun y1 (line)
  (cadr line))

(defun x2 (line)
  (caddr line))

(defun y2 (line)
  (cadddr line))


(defun 2point-to-line (p1 p2)
  (line (x1 p1) (y1 p1) (x1 p2) (y1 p2)))

(defun line-to-2point (line)
  (values (point (x1 line) (y1 line)) (point (x2 line) (y2 line))))

;;;; lib
(defun cons-if-not-nil (se1 se2)
  (if se1
      (cons se1 se2)
      se2))

(defun square (x)
  (* x x))


(defun distance (p1 p2)
  (sqrt (+ (square (- (x1 p1) (x1 p2)))
           (square (- (y1 p1) (y1 p2))))))


(defun coordinate-to-dir (start-point end-point)
  (atan (- (x1 end-point) (x1 start-point)) (- (y1 end-point) (y1 start-point))))

(defun line-center (line)
  (point (/ (+ (x1 line) (x2 line)) 2)
         (/ (+ (y1 line) (y2 line)) 2)))

(defun line-equal (line1 line2)
  (or (equal line1 line2)
      (equal line1 (line (x2 line2) (y2 line2) (x1 line2) (y1 line2)))))
  

(defparameter *angle-line-criteria*
  (line 0 0 0 10))

(defparameter *2PI* (* PI 2))

;;;; search


(defparameter *test-lines*
  '((10 20 20 20)
    (20 20 20 10)
    (20 10 10 20)
    (10 20 20 40)
    (20 20 20 40)))


(defun adjacent (point adj-points)
  (list point adj-points))

(defun find-next (point graph-lines)
  (mapcar #'caadr
          (remove-if-not #'(lambda (x) (equalp (car x) point)) graph-lines)))



(defun lines-to-graph-lines (lines)
  (apply #'append
         (mapcar #'(lambda (line)
                     (multiple-value-bind (p1 p2)
                         (line-to-2point line)
                       (list (adjacent p1 (list p2))
                             (adjacent p2 (list p1)))))
                 lines)))

(defun angle (start-point center-point end-point)
  (let ((st-angle (coordinate-to-dir start-point center-point))
        (ed-angle (coordinate-to-dir end-point center-point)))
    (- st-angle ed-angle (if (< st-angle ed-angle) (* -2 PI) 0))))

(defmacro func-angle (name func)
  `(defun ,name (before-point center-point next-points)
     (if (null next-points)
         nil
         (reduce #'(lambda (x y)
                     (if (,func (angle before-point center-point x)
                                (angle before-point center-point y))
                         x y))
                 next-points))))
  
(func-angle min-angle <)
(func-angle max-angle >)

(defun dfs (lines now-point ago-point goal-point path)
  (cond
    ((equalp now-point goal-point) (reverse (cons now-point path)))
    ((null now-point) nil)
    (t (let ((next-lines (remove-if #'(lambda (adj)
                                        (equalp (car adj) now-point))
                                    lines )))
         (dfs next-lines
              (max-angle ago-point now-point (find-next now-point lines))
              now-point goal-point (cons now-point path))))))


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

(defun points-angle (path &optional (angle nil))
  (cond
    ((null angle) (points-angle (append  (last path) path (list (car path))) 0))
    ((null (cddr path)) angle)
    (t (points-angle (cdr path)
                     (+ angle (angle (car path) (cadr path) (caddr path)))))))


(defun call-search (lines)
  (remove-if
   #'(lambda (path)
       (or (null path)
           (> (/ (points-angle path) (length path))
              (/ PI 3) )))
   (call-search+ lines)))
             

(defun search-2path (graph-lines line)
  (multiple-value-bind (p1 p2)
      (line-to-2point line)
    (values
     (dfs graph-lines p2 p1 p1 '())
     (dfs graph-lines p1 p2 p2 '()))))


