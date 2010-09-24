(in-package #:cl-user)

(defpackage #:standard.collection.hash.values
  (:nicknames #:std.collection.hash.values #:collection.hash.values #:hash.values)
  (:use #:cl #:std.collection.hash)
  (:import-from #:std.base #:defun-alias #:compose)
  (:shadowing-import-from #:hash #:map #:get #:length)
  (:shadow #:do #:count #:count-if #:count-if-not #:reduce #:find #:find-if #:find-if-not #:remove #:remove!)
  (:export
   #:do
   #:count
   #:count-if
   #:count-if-not
   #:reduce
   #:find
   #:find-if
   #:find-if-not
   #:remove
   #:remove!))