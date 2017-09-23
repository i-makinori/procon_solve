
(in-package :cl-user)
(defpackage procon-asd
  (:use :cl :asdf))
(in-package :procon-asd)


;; " procon-system wich doesn't use gui"
(defsystem procon
  :name "procon"
  :author "makinori"
  :version "0.00"
  :maintainer ""
  :licence "MIT"
  :description "hoge"
  :long-description "hoge"
  :depends-on (#:mcclim
               #:cl-json)
  :components
  ((:file "package")
   (:file "src/structure")
   (:file "src/io" :depends-on ("src/structure"))
   (:file "src/gui" :depends-on ("src/structure"))
   (:file "src/test")
   (:file "src/main")
   ))
