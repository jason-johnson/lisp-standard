(in-package #:cl-user)

(defpackage #:standard.collection.vector
  (:nicknames #:std.collection.vector #:collection.vector #:vector)
  (:use #:std.collection #:std.collection.array #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection #:remove #:map)
  (:shadowing-import-from #:std.collection.array #:copy #:do)
  (:shadow #:get #:put! #:make)
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
   #:has-fill-pointer
   #:adjustable-p))