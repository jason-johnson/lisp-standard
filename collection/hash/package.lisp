(in-package #:cl-user)

(defpackage #:standard.collection.hash
  (:nicknames #:std.collection.hash #:collection.hash #:hash)
  (:use #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadow #:get #:put! #:copy #:make #:remove #:remove! #:do #:length #:map)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:map
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