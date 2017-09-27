
(in-package :cl-user)
(defpackage procon-asd
  (:use :cl :asdf))
(in-package :procon-asd)


;; " procon-system wich doesn't use gui"
(defsystem procon-2016
  :name "procon-2016"
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
             (:file "command")
             (:file "svg")
             (:file "profile")
             (:file "main")))))
