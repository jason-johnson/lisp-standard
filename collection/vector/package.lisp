(in-package #:cl-user)

(defpackage #:standard.collection.vector
  (:nicknames #:std.collection.vector #:collection.vector #:vector)
  (:use #:std.collection.array #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias)
  (:import-from #:std.collection #:map-to)
  (:shadowing-import-from #:cl #:find #:find-if #:find-if-not #:length)
  (:shadowing-import-from #:std.collection.array #:copy)
  (:shadow #:get #:do #:map #:sort #:stable-sort #:merge #:concatenate)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:do
   #:map
   #:concatenate
   #:length
   #:rank
   #:dimension
   #:dimensions
   #:total-size
   #:element-type
   #:in-bounds-p
   #:displacement
   #:has-fill-pointer-p
   #:adjustable-p
   #:adjust
   #:push!
   #:push-extend!
   #:pop!
   #:reduce
   #:find
   #:find-if
   #:find-if-not
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