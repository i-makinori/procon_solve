
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
            ((:file "vector")
             (:file "struct")
             (:file "piece")))
   (:module "src"
            :components
            ((:file "game")))))
  
