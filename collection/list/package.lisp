(in-package #:cl-user)

(defpackage #:standard.collection.list
  (:nicknames #:std.collection.list #:collection.list #:list)
  (:use #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadow #:get #:put! #:copy #:do #:map)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:deep-copy
   #:do
   #:length
   #:map
   #:map!
   #:maplist
   #:maplist!
   #:foreach
   #:foreach-list
   #:map
   #:map!
   #:maplist
   #:maplist!
   #:foreach
   #:foreach-list))