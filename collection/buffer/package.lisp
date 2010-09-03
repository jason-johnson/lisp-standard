(in-package #:cl-user)

(defpackage #:standard.collection.buffer
  (:nicknames #:std.collection.buffer #:collection.buffer #:buffer)
  (:use #:std.collection.array #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection.array #:copy #:do)
  (:shadow #:get #:put! #:make)
  (:export
   #:make
   #:get
   #:put!
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
   #:adjustable-p
   #:remove
   #:remove^
   #:remove-if
   #:remove-if^
   #:remove-if-not
   #:remove-if-not^
   #:remove-duplicates
   #:remove-duplicates^
   #:reverse
   #:reverse^
   #:substitute
   #:substitute!
   #:substitute-if
   #:substitute-if!
   #:substitute-if-not
   #:substitute-if-not!
   #:append
   #:append!
   #:sort
   #:sort^))