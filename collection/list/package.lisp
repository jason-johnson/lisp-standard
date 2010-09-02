(in-package #:cl-user)

(defpackage #:standard.collection.list
  (:nicknames #:std.collection.list #:collection.list #:list)
  (:use #:std.collection #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection #:remove #:map)
  (:shadow #:get #:put! #:copy #:do)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:deep-copy
   #:do))