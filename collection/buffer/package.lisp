(in-package #:cl-user)

(defpackage #:standard.collection.buffer
  (:nicknames #:std.collection.buffer #:collection.buffer #:buffer)
  (:use #:std.collection.vector #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias)
  (:import-from #:std.collection #:map-to)
  (:shadowing-import-from #:std.collection.vector #:copy #:do #:sort #:stable-sort #:merge^ #:merge)
  (:shadow #:get #:put! #:make #:concatenate #:map)
  (:export
   #:make
   #:get
   #:put!
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
   #:adjustable-p
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