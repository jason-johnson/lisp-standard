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
				     (:file "collection" :depends-on ("package"))
				     (:module list
					      :components ((:file "package")
							   (:file "list" :depends-on ("package")))
					      :depends-on ("package" "collection"))
				     (:module array
					      :components ((:file "package")
							   (:file "array" :depends-on ("package")))
					      :depends-on ("package" "collection"))
				     (:module vector
					      :components ((:file "package")
							   (:file "vector" :depends-on ("package")))
					      :depends-on ("package" "collection" #:array))
				     (:module hash
					      :components ((:file "package")
							   (:file "hash" :depends-on ("package")))
					      :depends-on ("package" "collection"))
				     (:file "aliases" :depends-on (#:list #:array))
				     (:file "pretty-print" :depends-on (#:hash)))
			:depends-on (#:base))
	       (:file "package" :depends-on (#:base #:collection))))