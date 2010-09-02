(in-package #:cl-user)

(defpackage #:standard.collection.set
  (:nicknames #:std.collection.set #:collection.set #:set)
  (:use #:std.collection #:std.collection.hash #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection #:map)
  (:shadow #:get #:put! #:copy #:remove #:remove! #:do)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:remove
   #:remove!
   #:do
   #:length
   #:clear
   #:setp
   #:size
   #:test
   #:rehash-size
   #:rehash-threshold
   #:hash))