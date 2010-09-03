(in-package #:cl-user)

(defpackage #:standard.collection.set
  (:nicknames #:std.collection.set #:collection.set #:set)
  (:use #:std.collection #:std.collection.hash #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection #:map)
  (:shadow #:make #:get #:put! #:clear #:size #:test #:rehash-size #:rehash-threshold #:length #:options #:copy #:do #:remove #:remove! #:set #:union #:intersection #:complement #:subsetp)
  (:export
   #:set
   #:setp
   #:make
   #:memberp
   #:add
   #:copy
   #:remove
   #:remove!
   #:do
   #:length
   #:clear
   #:size
   #:test
   #:rehash-size
   #:rehash-threshold
   #:hash))