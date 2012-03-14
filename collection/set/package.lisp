(in-package #:cl-user)

(defpackage #:standard.collection.set
  (:nicknames #:std.collection.set #:collection.set #:set)
  (:use #:std.collection.hash #:cl)
  (:import-from #:std.base #:defun-alias #:compose)
  (:shadow
   #:make
   #:get
   #:put!
   #:map
   #:do
   #:clear
   #:size
   #:test
   #:rehash-size
   #:rehash-threshold
   #:length
   #:options
   #:copy
   #:count
   #:count-if
   #:count-if-not
   #:reduce
   #:find
   #:find-if
   #:find-if-not
   #:remove
   #:remove!
   #:set
   #:union
   #:intersection
   #:complement
   #:subsetp)
  (:export
   #:set
   #:setp
   #:make
   #:member?
   #:add
   #:copy
   #:remove
   #:remove!
   #:do
   #:map
   #:count
   #:count-if
   #:count-if-not
   #:reduce
   #:find
   #:find-if
   #:find-if-not
   #:length
   #:clear
   #:size
   #:test
   #:rehash-size
   #:rehash-threshold
   #:options
   #:hash
   #:union!
   #:union
   #:intersection!
   #:intersection
   #:complement!
   #:complement
   #:cartesian-product
   #:subsetp))