
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
   (:module "util"
            :components
            ((:file "util")
             (:file "maybe-nothing")
             (:file "tree")
             (:file "line")
             (:file "vector")))
   (:module "lib"
            :components
            (;; piece
             (:file "piece")
             ;; synth
             (:file "synth")
             (:file "synthesize")
             ;; search 1-move / search lib
             (:file "search-util")
             (:file "search")))
   (:module "src"
            :components
            (;; application
             (:file "io")
             ;; gui
             (:file "gui")
             (:file "run-gui")
             ;; decrim
             ;; app
             (:file "test")
             (:file "main")))
   (:module "test"
            :components
            ((:file "samples1")
             (:file "samples2")))
   ))
