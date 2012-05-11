(in-package #:cl-user)

(defpackage #:standard.collection.hash
  (:nicknames #:std.collection.hash #:collection.hash #:hash)
  (:use #:cl)
  (:import-from #:std.base #:defun-alias #:compose)
  (:shadow #:get #:put! #:copy #:make #:count #:count-if #:reduce #:find #:find-if #:remove #:remove! #:do #:length #:map)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:map
   #:count
   #:count-if
   #:reduce
   #:find
   #:find-if
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
   #:hash
   #:options))