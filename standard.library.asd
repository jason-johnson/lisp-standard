;;; -*- lisp -*- system definition

(in-package #:asdf)

(defsystem #:standard.library
  :description "Library system for standard (see system 'standard for details)"
  :depends-on (#:standard.core)
  :components ((:module libraries
			:components ((:module net
					      :components ((:file "package")
							   (:file "net" :depends-on ("package"))))))))
;				     (:file "package")))))