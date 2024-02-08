
(in-package :cl-user)
(defpackage puzzle-1617-asd
  (:use :cl :asdf))
(in-package :puzzle-1617-asd)


;; " procon-system wich doesn't use gui"
(defsystem puzzle-1617
  :name "puzzle-1617"
  :author "makinori"
  :version "1"
  :maintainer ""
  :licence "MIT"
  :description "puzzle-1617 solver"
  :long-description "puzzle-1617 solver. 2023 version."
  :depends-on (#:cl-json)
  :components
  (;; package
   (:file "package")
   (:module "src"
            :components
            (;; utility
             (:file "utility")
             ;; define of types
             (:file "defines") ;; "data-types"
             ;; solver
             (:file "linear-algebra")
             (:file "solver")
             ;; I/O, viewer render
             (:file "io")
             ;; SVG viwer
             (:file "render-html")
             ;; test
             ))
   ))
