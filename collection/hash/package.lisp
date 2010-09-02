(in-package #:cl-user)

(defpackage #:standard.collection.hash
  (:nicknames #:std.collection.hash #:collection.hash #:hash)
  (:use #:std.collection #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection #:map)
  (:shadow #:get #:put! #:copy #:make #:remove #:remove! #:do #:length)
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
   #:hashp
   #:size
   #:test
   #:rehash-size
   #:rehash-threshold
   #:hash))