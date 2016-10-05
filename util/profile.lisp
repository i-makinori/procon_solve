
(in-package #:procon)


(defun profile-procon ()
  (sb-profile:unprofile)
  (sb-profile:profile "PROCON" "CL-USER")
  (time
   (dotimes (x 10000) (synthesize-piece *test-condi10* *test-condi11*))
   )
  (sb-profile:report)
  (sb-profile:unprofile))


