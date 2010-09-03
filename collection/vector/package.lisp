(in-package #:cl-user)

(defpackage #:standard.collection.vector
  (:nicknames #:std.collection.vector #:collection.vector #:vector)
  (:use #:std.collection.array #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias)
  (:import-from #:std.collection #:map-to)
  (:shadowing-import-from #:std.collection.array #:copy #:do)
  (:shadow #:get #:put! #:push #:pop #:map #:sort)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:do
   #:map
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
   #:adjust
   #:push
   #:push-extend
   #:pop
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