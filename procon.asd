
(ql:quickload :lispbuilder-sdl)

(in-package :cl-user)
(defpackage procon-asd
  (:use :cl :asdf))
(in-package :procon-asd)


(defsystem procon
  :name "procon"
  :author "makinori"
  :version "0.00"
  :maintainer ""
  :licence "GPL"
  :description "hoge"
  :long-description "hoge"
  :components
  ((:file "package")
   (:module "lib"
            :components
            ((:file "util")
             (:file "standard-error")
             (:file "lazy")
             (:file "line")
             (:file "vector")
             (:file "struct")
             (:file "piece")
             (:file "synth-piece")
             (:file "test")))
   (:module "src"
            :components
            ((:file "game")
             (:file "rule")
             (:file "search")
             (:file "gui")
             (:file "profile")))))
  
