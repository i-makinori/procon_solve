
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
  :description "procon solve"
  :long-description "procon solve"
  :depends-on (#:mcclim #:cl-json)
  :components
  (;; package
   (:file "package")
   ;; test :: samples
   (:file "test/samples")
   ;; src.libs
   (:file "src/util" :depends-on ("test/samples"))
   (:file "src/geometry" :depends-on ("src/util"))
   (:file "src/structure" :depends-on ("src/util" "src/geometry"))
   ;; src.application
   (:file "src/io" :depends-on ("src/structure"))
   (:file "src/gui" :depends-on ("src/structure" "src/io"))
   (:file "src/test")
   (:file "src/main")
   ))
