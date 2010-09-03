(in-package #:cl-user)

(defpackage #:standard.collection.set
  (:nicknames #:std.collection.set #:collection.set #:set)
  (:use #:std.collection.hash #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadow #:make #:get #:put! #:map #:do #:clear #:size #:test #:rehash-size #:rehash-threshold #:length #:options #:copy #:remove #:remove! #:set #:union #:intersection #:complement #:subsetp)
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
   #:map
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