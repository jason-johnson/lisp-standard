(in-package #:cl-user)

(defpackage #:standard.collection.bit-array
  (:nicknames #:std.collection.bit-array #:collection.bit-array #:bit-array)
  (:use #:std.collection.array #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection.array
			  #:get
			  #:map
			  #:copy
			  #:do
			  #:length
			  #:reduce
			  #:concatenate
			  #:count
			  #:count-if
			  #:find
			  #:find-if
			  #:position
			  #:position-if
			  #:substitute
			  #:substitute-if
			  #:substitute!
			  #:substitute-if!
			  #:append!
			  #:reverse
			  #:reverse!
			  #:sort
			  #:sort^
			  #:stable-sort
			  #:merge^
			  #:merge)
  (:shadow #:make #:and #:or #:not)
  (:export
   #:make
   #:get
   #:map
   #:copy
   #:do
   #:length
   #:rank
   #:dimension
   #:dimensions
   #:total-size
   #:element-type
   #:in-bounds-p
   #:displacement
   #:row-major-index
   #:has-fill-pointer
   #:fill-pointer
   #:adjustable-p
   #:and
   #:nand
   #:or
   #:nor
   #:xor
   #:xnor
   #:not
   #:andc1
   #:andc2
   #:orc1
   #:orc2
   #:and!
   #:nand!
   #:or!
   #:nor!
   #:xor!
   #:xnor!
   #:not!
   #:andc1!
   #:andc2!
   #:orc1!
   #:orc2!
   #:and-into!
   #:nand-into!
   #:or-into!
   #:nor-into!
   #:xor-into!
   #:xnor-into!
   #:not-into!
   #:andc1-into!
   #:andc2-into!
   #:orc1-into!
   #:orc2-into!
   #:reverse				; TODO: I think all structural modifying functions are gone, now this needs to be done to array/package.lisp and bit-vector needs to inherit from bit-array and vector
   #:reverse!
   #:substitute
   #:substitute!
   #:substitute-if
   #:substitute-if!
   #:append
   #:append!
   #:sort
   #:sort^
   #:stable-sort
   #:stable-sort^
   #:merge
   #:merge^))