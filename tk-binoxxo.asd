;;;; tk-binoxxo.asd

(asdf:defsystem #:tk-binoxxo
  :serial t
  :description "A TK wrapper for my binoxxo puzzle solver."
  :author "Martin Buchmann <Martin.Buchmann@gmail.com>"
  :license "Free for all!"
  :depends-on (#:alexandria
               #:ltk)
  :components ((:file "package")
	       (:file "debug")
               (:file "binoxxo-engine")
	       (:file "tk-binoxxo")))

