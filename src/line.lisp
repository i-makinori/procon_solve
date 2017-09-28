(in-package :procon)


;;;; line, etc ;;;;;;;;;;;;;;



;;;; structure point
(defun point (x y)
  `((:x ,x) (:y ,y)))

(defun p-x (point)
  "point-x"
  (car-rest-assoc :x point))

(defun p-y (point)
  "point-y"
  (car-rest-assoc :y point))


;;;; structure line
  
(defun line (x1 y1 x2 y2)
  `((:x1 ,x1) (:y1 ,y1)
    (:x2 ,x2) (:y2 ,y2)))

(defun line-x1 (line)
  (car-rest-assoc :x1 line))

(defun line-y1 (line)
  (car-rest-assoc :y1 line))

(defun line-x2 (line)
  (car-rest-assoc :x2 line))

(defun line-y2 (line)
  (car-rest-assoc :y2 line))


(defun 2point->line (p1 p2)
  (line (p-x p1) (p-y p1) (p-x p2) (p-y p2)))

(defun line->2point (line)
  (values (point (line-x1 line) (line-y1 line)) (point (line-x2 line) (line-y2 line))))



;;;; tests
(defparameter *test-point1* (point 10 20))
(defparameter *test-point2* (point 60 80))
(defparameter *test-point3* (point -40 10))

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
  (sqrt (+ (square (- (p-x p1) (p-x p2)))
           (square (- (p-y p1) (p-y p2))))))


(defun coordinate-to-dir (start-point end-point)
  (atan (- (p-x end-point) (p-x start-point)) 
        (- (p-y end-point) (p-y start-point))))

(defun line-center (line)
  (point (/ (+ (line-x1 line) (line-x2 line)) 2)
         (/ (+ (line-y1 line) (line-y2 line)) 2)))

(defun line-equal (line1 line2)
  (or (equal line1 line2)
      (equal line1 (line (line-x2 line2) (line-y2 line2) (line-x1 line2) (line-y1 line2)))))
  

(defparameter *angle-line-criteria*
  (line 0 0 0 1)
  "unit origin of degree as line-type")

(defparameter *2PI* (* PI 2))

;;;; search

(defun adjacent (point adj-points)
  (list point adj-points))

(defun find-next (point graph-lines)
  (mapcar #'caadr
          (remove-if-not #'(lambda (x) (equalp (car x) point)) graph-lines)))



(defun lines-to-graph-lines (lines)
  (apply #'append
         (mapcar #'(lambda (line)
                     (multiple-value-bind (p1 p2)
                         (line->2point line)
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
      (line->2point line)
    (values
     (dfs graph-lines p2 p1 p1 '())
     (dfs graph-lines p1 p2 p2 '()))))
