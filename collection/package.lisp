(in-package #:cl-user)

(defpackage #:standard.collection
  (:nicknames #:std.collection #:collection)
  (:use #:cl #:std.base)
  (:shadow #:get #:count #:count-if #:count-if-not #:reduce #:find #:find-if #:find-if-not #:remove #:map #:remove-if #:remove-if-not #:remove-duplicates #:reverse #:fill #:substitute #:substitute-if #:substitute-if-not #:append #:sort #:stable-sort #:merge)
  (:export
   #:get
   #:put!
   #:copy
   #:remove
   #:remove^
   #:map-to
   #:map
   #:count
   #:count-if
   #:count-if-not
   #:reduce
   #:find
   #:find-if
   #:find-if-not
   #:remove-if
   #:remove-if^
   #:remove-if-not
   #:remove-if-not^
   #:remove-duplicates
   #:remove-duplicates^
   #:reverse
   #:reverse^
   #:fill
   #:substitute
   #:substitute!
   #:substitute-if
   #:substitute-if!
   #:substitute-if-not
   #:substitute-if-not!
   #:append
   #:append!
   #:sort
   #:sort^
   #:stable-sort
   #:stable-sort^
   #:merge
   #:merge^))