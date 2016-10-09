
(ql:quickload :lispbuilder-sdl)

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
             (:file "piece")
             (:file "synth-piece")
             (:file "test")))
   (:module "src"
            :components
            ((:file "repl")
             (:file "search")
             (:file "io")
             (:file "command")))
   (:module "tools"
            :components
            ((:file "gui")
             (:file "svg")
             (:file "profile")
             (:file "main"))
   )))

  
#|
;;  "procon-system which uses gui " 
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
             (:file "piece")
             (:file "synth-piece")
             (:file "test")))
   (:module "src"
            :components
            ((:file "game")
             (:file "rule")
             (:file "search")
             (:file "gui")
             (:file "profile")
             (:file "main")))))
  
|#
