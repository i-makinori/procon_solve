(in-package :procon)

;;;; synthesize ;;;;;;;;;;;;;;;;

#|
(defun synthesize-easy-piece-rule (easy-piece1 easy-piece2)
  (let* ((hoge))
    hoge
    ))

(defun easy-piece->vdr-list (easy-piece1 easy-piece2)
  
  )
|#


(defstruct vdr
  (vec (vec 0 0))
  (deg 0))

(defun vdr-vec-origin-shift (vdr-vecs)
  (let ((first-vec (car vdr-vecs)))
    (mapcar #'(lambda (vdc)
                (vec-sub  vdc first-vec))
            vdr-vecs)))

(defun vdr-to-easy-piece (vdr-list)
  (easy-piece (vecs->spots (vdr-vec-origin-shift (mapcar #'vdr-vec vdr-list)))
              (mapcar #'vdr-deg vdr-list)
              nil))

(defun synthesize-easy-piece-rule (easy-piece1 easy-piece2)
  (let* ((vdr-queue
          (vdr-to-vdr-queue
           (easy-piece-to-vdr easy-piece1)
           (easy-piece-to-vdr easy-piece2)))
         (vdr-added-line-point-vdr1
          (add-ridden-line-point (car vdr-queue) (cdr vdr-queue)))
         (vdr-added-line-point-vdr2
          (add-ridden-line-point (cdr vdr-queue) (car vdr-queue)))
         (remove-sy-line
          (remove-sy-line vdr-added-line-point-vdr1 vdr-added-line-point-vdr2)))
    (if (position nil remove-sy-line) nil
        (vdr-to-easy-piece (remove-line
                            (remove-synth-rev (remove-synth remove-sy-line)))))))

(defun remove-line (vdr-list)
  (remove-if
   #'(lambda (vdr) (pi-judge-contain-error (vdr-deg vdr)))
   vdr-list)
  )

(defun remove-round (vdr-list)
  (remove-if #'(lambda (vdr) (2pi-judge-contain-error (vdr-deg vdr))) vdr-list))


(defun remove-synth-rev (vdr-list)
  "rev:reverse direction of list"
  (let ((vdr-synth (synthesize-vdr (car vdr-list) (rotate-nth -1 vdr-list))))
    (cond ((2pi-judge-contain-error (vdr-deg (car vdr-list)))
           (remove-synth-rev (cdr vdr-list)))
          (vdr-synth
           (remove-synth-rev (cons vdr-synth (init (cdr vdr-list)))))
          (t vdr-list))))

(defun remove-synth (vdr-list &optional (s-vdr nil))
  (let ((vdr-synth (synthesize-vdr (car vdr-list) (car s-vdr))))
    (cond ((null vdr-list) s-vdr)
          ((2pi-judge-contain-error (vdr-deg (car vdr-list)))
           (remove-synth (cdr vdr-list) s-vdr))
          ((and vdr-synth (2pi-judge-contain-error (vdr-deg vdr-synth)))
           (remove-synth (cdr vdr-list) (cdr s-vdr)))
          (vdr-synth
           (remove-synth (cons vdr-synth (cdr vdr-list))
                         (cdr s-vdr)))
          ('t
           (remove-synth (cdr vdr-list) (cons (car vdr-list) s-vdr))))))


(defun remove-sy-line (vdr1 vdr2)
  "the line across origin point to last point of piece16-"
  (let ((remove-origin
         (append (drop vdr1 1)
                 (cons (synthesize-vdr (car vdr2) (car vdr1))
                       (reverse (cdr vdr2))))))
    (cons (synthesize-vdr (car remove-origin) (car (last remove-origin)))
          (cdr (init remove-origin)))))


(defun synthesize-vdr (vdr1 vdr2)
  (if (and vdr1 vdr2
           (vec= (vdr-vec vdr1) (vdr-vec vdr2)))
      (make-vdr :vec (vec-prod (vec-add (vdr-vec vdr1) (vdr-vec vdr2))
                               (vec 1/2 1/2))
                :deg (+ (vdr-deg vdr1) (vdr-deg vdr2)))))

(defun add-ridden-line-point-adjust (vdr-list &optional (s-vdr-list '()))
  (cond ((null (cdr vdr-list)) s-vdr-list)
        ((or (null s-vdr-list)
             (not (vec= (vdr-vec (car vdr-list)) (vdr-vec (cadr vdr-list)))))
         (add-ridden-line-point-adjust
          (cdr vdr-list) (cons (car vdr-list) s-vdr-list)))
        (t (add-ridden-line-point-adjust (cdr vdr-list) s-vdr-list))))


(defun add-ridden-line-point (vdr1 vdr2 &optional (s-vdr 'F))
  ;; rewrite-able to reduce form 
  (cond ((null (cdr vdr1)) (reverse
                            (add-ridden-line-point-adjust (append-car-last
                                                           (reverse s-vdr)))))
        ((eq 'F s-vdr) (add-ridden-line-point (append-car-last vdr1) vdr2 nil))
        ('t (let ((this-point-vec (vdr-vec (car vdr1)))
                  (next-point-vec (vdr-vec (cadr vdr1))))
              (add-ridden-line-point
               (cdr vdr1) vdr2
               (append
                (mapcar #'(lambda (p)
                            (make-vdr :vec (vdr-vec p)
                                      :deg  pi))
                        (remove-if-not
                         #'(lambda (p)
                             (and (not (vec= (vdr-vec p) this-point-vec))
                                  (not (vec= (vdr-vec p) next-point-vec))
                                  (line-spot-vec-hit-judge-error
                                   (vector-to-line this-point-vec next-point-vec)
                                   (vdr-vec p))))
                         vdr2))
                (list (car vdr1)) s-vdr ))))))


(defun vdr-to-vdr-queue (vdr1 vdr2 &key (flag nil) (time 0))
  "let direction of round correct"
  (if (same-vector-angle (vdr-vec (cadr vdr1)) (vdr-vec (cadr vdr2)))
                                        ;same-v-ang :: include std-error
      (cons vdr1 vdr2)
      (cond ((>= time 3) (error "vdr-to-vdr-queue error"))
            (flag (vdr-to-vdr-queue vdr1 (cons (car vdr2) (reverse (cdr vdr2)))
                                    :flag nil :time (1+ time)))
            (t (vdr-to-vdr-queue (cons (car vdr1) (reverse (cdr vdr1))) vdr2
                                 :flag t :time (1+ time))))))


(defun easy-piece-to-vdr (easy-piece)
  (mapcar #'(lambda (vec deg) (make-vdr :vec vec :deg deg))
          (spots->vecs (epiece-spots easy-piece))
          (epiece-degrees easy-piece)))
