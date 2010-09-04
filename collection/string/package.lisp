(in-package #:cl-user)

(defpackage #:standard.collection.string
  (:nicknames #:std.collection.string #:collection.string #:string)
  (:use #:std.collection.vector #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection.array #:copy #:do)
  (:shadowing-import-from #:std.collection.vector #:get #:map #:sort #:stable-sort #:merge^ #:merge)
  (:shadow #:make #:concatenate)
  (:export
   #:make
   #:get
   #:put!
   #:map
   #:concatenate
   #:copy
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
   #:sort^
   #:stable-sort
   #:stable-sort^
   #:merge
   #:merge^))