(in-package #:cl-user)

(defpackage #:standard.collection.array
  (:nicknames #:std.collection.array #:collection.array #:array)
  (:use #:cl)
  (:import-from #:std.base #:defun-alias #:compose)
  (:shadow #:get #:put! #:map #:count #:count-if #:count-if-not #:reduce #:find #:find-if #:find-if-not #:length #:copy #:do)
  (:export
   #:make
   #:get
   #:put!
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
   #:row-major-put!))