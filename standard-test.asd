;;; -*- lisp -*- system definition

(in-package #:asdf)

(defsystem #:standard-test
  :description "Test package for standard library"
  :depends-on (#:standard #:lift)
  :components ((:module :test
			:components ((:file "package")
				     (:file "test" :depends-on ("package"))))))
