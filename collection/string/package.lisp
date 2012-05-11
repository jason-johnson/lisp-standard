(in-package #:cl-user)

(defpackage #:standard.collection.string
  (:nicknames #:std.collection.string #:collection.string #:string)
  (:use #:std.collection.vector #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection.vector #:copy #:do #:concatenate #:get #:map #:sort #:stable-sort #:merge^ #:merge)
  (:shadow #:make)
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