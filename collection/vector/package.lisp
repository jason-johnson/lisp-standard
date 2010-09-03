(in-package #:cl-user)

(defpackage #:standard.collection.vector
  (:nicknames #:std.collection.vector #:collection.vector #:vector)
  (:use #:std.collection.array #:cl)
  (:import-from #:std.base #:defun-alias)
  (:import-from #:std.collection #:map-to)
  (:shadowing-import-from #:std.collection.array #:copy #:do)
  (:shadow #:get #:put! #:push)
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
   #:push
   #:push-extend))