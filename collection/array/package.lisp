(in-package #:cl-user)

(defpackage #:standard.collection.array
  (:nicknames #:std.collection.array #:collection.array #:array)
  (:use #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadow #:get #:put! #:copy #:do)
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
   #:has-fill-pointer-p
   #:fill-pointer
   #:adjustable-p
   #:row-major-get
   #:major-row-put!))