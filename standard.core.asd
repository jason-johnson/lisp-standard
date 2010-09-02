;;; -*- lisp -*- system definition

(in-package #:asdf)

(defsystem #:standard.core
  :description "Core system for standard (see system 'standard for details)"
  :components ((:module base
			:components ((:file "package")
				     (:file "base" :depends-on ("package")))
			:depends-on ())
	       (:module collection
			:components ((:file "package")
				     (:file "collection" :depends-on ("package")))
			:depends-on (#:base))
	       (:file "package" :depends-on (#:base #:collection))))