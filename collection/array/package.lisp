(in-package #:cl-user)

(defpackage #:standard.collection.array
  (:nicknames #:std.collection.array #:collection.array #:array)
  (:use #:std.collection #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection #:remove #:map)
  (:shadow #:get #:put! #:copy #:do)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:do
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
   #:row-major-get
   #:major-row-put!))