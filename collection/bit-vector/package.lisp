(in-package #:cl-user)

(defpackage #:standard.collection.bit-vector
  (:nicknames #:std.collection.bit-vector #:collection.bit-vector #:bit-vector)
  (:use #:standard.collection.bit-array #:std.collection.vector #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:standard.collection.bit-array #:make #:and #:or #:not)
  (:shadowing-import-from #:std.collection.vector
			  #:map
			  #:copy
			  #:do
			  #:length
			  #:concatenate
			  #:substitute!
			  #:substitute-if!
			  #:append!
			  #:reverse
			  #:reverse^
			  #:sort
			  #:sort^
			  #:stable-sort
			  #:merge^
			  #:merge)
  (:shadow #:get #:put!)
  (:export
   #:make
   #:get
   #:put!
   #:map
   #:concatenate
   #:copy
   #:copy^
   #:do
   #:push!
   #:push-extend!
   #:pop!
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
   #:remove
   #:remove^
   #:remove-if
   #:remove-if^
   #:filter
   #:filter^
   #:remove-duplicates
   #:remove-duplicates^
   #:reverse
   #:reverse^
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