(defpackage :musicdb-system
  (:use :cl :asdf))

(in-package :musicdb-system)

(defsystem "musicdb"
    :description "musicdb: a sample Lisp system."
    :version "0.2.1"
    :author "Leslie Chu <pythonisland@gmail.com>"
    :licence "Public Domain"
    :components ((:file "musicdb")
                 (:file "packages" :depends-on ("musicdb"))))
