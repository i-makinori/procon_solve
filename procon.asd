
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

   ;; src.libs / src.structure
   (:file "src/util" )
   (:file "src/line" :depends-on ("src/util"))
   (:file "src/vector" :depends-on ("src/util" "src/line"))

   ;; piece-16
   ;; synth-piece
   (:file "src/synthesize")
   (:file "src/piece")


   ;; test :: samples
   (:file "test/samples1")
   (:file "test/samples2")

   ;; src.synth

   ;; src.search
   
   ;; src.application
   (:file "src/io")
   (:file "src/gui")
   ;; decrim

   (:file "src/synth" :depends-on ("src/util" "src/piece"))

   ;; app
   (:file "src/test")
   (:file "src/main")
   ))
