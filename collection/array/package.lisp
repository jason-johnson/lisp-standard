(in-package #:cl-user)

(defpackage #:standard.collection.array
  (:nicknames #:std.collection.array #:collection.array #:array)
  (:use #:cl)
  (:import-from #:std.base #:defun-alias #:compose)
  (:shadow #:get #:map #:count #:count-if #:count-if-not #:reduce #:find #:find-if #:find-if-not #:length #:copy #:do)
  (:export
   #:make
   #:new-from
   #:get
   #:copy
   #:map
   #:do
   #:count
   #:count-if
   #:count-if-not
   #:reduce
   #:find
   #:find-if
   #:find-if-not
   #:length
   #:rank
   #:dimension
   #:dimensions
   #:total-size
   #:element-type
   #:in-bounds-p
   #:displacement
   #:row-major-index
   #:has-fill-pointer-p
   #:fill-pointer
   #:adjustable-p
   #:adjust
   #:row-major-get
   #:row-major-put!
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
   #:merge^))			; TODO: Try to add as many collection functions as possible.  Simply none that modify the structur.  Things like append! have to take compatible structures.