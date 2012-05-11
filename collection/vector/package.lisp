(in-package #:cl-user)

(defpackage #:standard.collection.vector
  (:nicknames #:std.collection.vector #:collection.vector #:vector)
  (:use #:std.collection.array #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias)
  (:import-from #:std.collection #:map-to)
  (:shadowing-import-from #:impl-common #:substitute! #:substitute-if! #:append! #:reverse^ #:sort^)
  (:shadowing-import-from #:cl #:count #:count-if #:reduce #:find #:find-if #:position #:position-if #:length)
  (:shadowing-import-from #:std.collection.array #:copy)
  (:shadow #:get #:do #:map #:sort #:stable-sort #:merge #:merge^ #:concatenate)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:copy^
   #:new-from
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
   #:fill-pointer
   #:adjustable-p
   #:adjust
   #:push!
   #:push-extend!
   #:pop!
   #:count
   #:count-if
   #:reduce
   #:find
   #:find-if
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