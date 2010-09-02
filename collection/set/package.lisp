(in-package #:cl-user)

(defpackage #:standard.collection.set
  (:nicknames #:std.collection.set #:collection.set #:set)
  (:use #:std.collection #:std.collection.hash #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection #:map)
  (:shadowing-import-from #:std.collection.hash #:length)
  (:shadow #:make #:get #:put! #:clear #:size #:test #:rehash-size #:rehash-threshold #:copy #:do #:remove #:remove! #:set #:union #:intersection #:complement #:subsetp)
  (:export
   #:make
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