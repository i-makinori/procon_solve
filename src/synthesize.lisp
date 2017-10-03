(in-package :procon)

;;;; synthesize ;;;;;;;;;;;;;;;;

;;;; structure 

;; vdr :: Vector-Degree-|:S:|
(defun vdr (vec deg)
  (cons vec deg))

(defun vdr-vec (vdr)
  (car vdr))

(defun vdr-deg (vdr)
  (cdr vdr))


;; Rotation list

(defun rotation-list (list- list+ &optional fai)
  (list fai list- list+))

(defun make-r-list (list- list+ &optional fai)
  (rotation-list list- list+ fai))

(defun r-list-fai (rlist)
  (car rlist))

(defun r-list- (rlist)
  (cadr rlist))

(defun r-list+ (rlist)
  (caddr rlist))

(defun r-hole (rlist)
  (make-r-list (reverse (r-list+ rlist))
               (reverse (r-list- rlist))
               (rlist-fai rlist)))

(defun r-list-cdr (rlist)
  (make-r-list  (cdr (r-list- rlist))
                (cdr (r-list+ rlist))
                (r-list-fai rlist)))

(defun func-r-list-car (func rlist)
  (funcall func
           (car (r-list- rlist))
           (car (r-list+ rlist))))
           
(defun r-list->list (rlist)
  ;; undefined
  )

(defparameter *test-rlist1*
  (rotation-list (reverse '(1 2 3 4 5)) '(6 7 8 9 10) 100))

;;;; functions 

(defun easy-piece-to-vdr-list (easy-piece)
  (mapcar #'vdr 
          (spots->vecs (epiece-spots easy-piece))
          (epiece-degrees easy-piece)))


(defun synthesize-easy-piece-rule (easy-piece1 easy-piece2)
  (let* ((vdr-list1 (easy-piece-to-vdr-list easy-piece1))
         (vdr-list2 (easy-piece-to-vdr-list easy-piece2))
         ;;
         (vdr-list1-add-line-point
          (add-ridden-line-point vdr-list1 vdr-list2))
         (vdr-list2-add-line-point
          (add-ridden-line-point vdr-list2 vdr-list1))
         ;;
         (rlist (make-r-list vdr-list1-add-line-point
                             vdr-list2-add-line-point)))
    (remove-vdrs rlist)))


;;;; remove points

(defun n-pi-test (deg)
  "deg = n * pi => t"
  (error-round-deg= 0 (mod deg pi)))
  

(defun when-same-vdr-vec (vdr1 vdr2)
  (let ((deg+deg (+ (vdr-deg vdr1) (vdr-deg vdr2)))
        (vec (vdr-vec vdr1))) ;; = (vdr-vec vdr2)
    (if (n-pi-test deg+deg) nil 
        (vdr vec deg+deg))))
        

(defun remove-vdrs (vdr-list--rlist)
  "rlist([vdr]) -> rlist([vdr])"
  (print vdr-list--rlist)
  (cond ((func-r-list-car #'(lambda (vdr1 vdr2) (vec= (vdr-vec vdr1) (vdr-vec vdr2)))
                          vdr-list--rlist)
         (remove-vdrs 
          (make-r-list (func-r-list-car #'when-same-vdr-vec  vdr-list--rlist)
                       (r-list-cdr vdr-list--rlist)))
        (t vdr-list--rlist))
         
         

)))
