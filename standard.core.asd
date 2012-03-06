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
				     (:file "aliases" :depends-on ("package"))
				     (:module impl-common
					      :components ((:file "package")
							   (:file "impl-common" :depends-on ("package"))
							   (:module unordered
								    :components ((:file "package")
										 (:file "unordered"))
								    :depends-on ()))
					      :depends-on ("package" "collection"))
				     (:module list
					      :components ((:file "package")
							   (:file "list" :depends-on ("package"))
							   (:file "generic" :depends-on ("package" "list")))
					      :depends-on ("package" "collection" #:impl-common))
				     (:module array
					      :components ((:file "package")
							   (:file "array" :depends-on ("package"))
							   (:file "generic" :depends-on ("package" "array")))
					      :depends-on ("package" "collection"))
				     (:module vector
					      :components ((:file "package")
							   (:file "vector" :depends-on ("package")))
					      :depends-on ("package" "collection" "aliases" #:array #:impl-common))
				     (:module buffer
					      :components ((:file "package")
							   (:file "buffer" :depends-on ("package")))
					      :depends-on ("package" "collection" "aliases" #:array #:vector #:impl-common))
				     (:module bit-vector
					      :components ((:file "package")
							   (:file "bit-vector" :depends-on ("package")))
					      :depends-on ("package" "collection" "aliases" #:array #:vector #:impl-common))
				     (:module string
					      :components ((:file "package")
							   (:file "string" :depends-on ("package")))
					      :depends-on ("package" "collection" "aliases" #:array #:vector #:impl-common))
				     (:module hash
					      :components ((:file "package")
							   (:file "hash" :depends-on ("package"))
							   (:module keys
								    :components ((:file "package")
										 (:file "keys" :depends-on ("package")))
								    :depends-on ("package" "hash"))
							   (:module values
								    :components ((:file "package")
										 (:file "values" :depends-on ("package")))
								    :depends-on ("package" "hash"))
							   (:file "generic" :depends-on ("package" "hash" #:values)))
					      :depends-on ("package" "collection" #:array #:vector))
				     (:module set
					      :components ((:file "package")
							   (:file "set" :depends-on ("package")))
					      :depends-on ("package" "collection" #:hash))
				     (:file "pretty-print" :depends-on (#:hash #:set)))
			:depends-on (#:base))
	       (:file "package" :depends-on (#:base #:collection))))