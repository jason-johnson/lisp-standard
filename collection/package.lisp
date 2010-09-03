(in-package #:cl-user)

(defpackage #:standard.collection
  (:nicknames #:std.collection #:collection)
  (:use #:cl #:std.base)
  (:shadow #:get #:remove #:map #:remove-if #:remove-if-not #:remove-duplicates #:reverse #:substitute #:substitute-if #:substitute-if-not #:append)
  (:export
   #:get
   #:put!
   #:copy
   #:remove
   #:remove^
   #:map-to
   #:map
   #:remove-if
   #:remove-if^
   #:remove-if-not
   #:remove-if-not^
   #:remove-duplicates
   #:remove-duplicates^
   #:reverse
   #:reverse^
   #:substitute
   #:substitute!
   #:substitute-if
   #:substitute-if!
   #:substitute-if-not
   #:substitute-if-not!
   #:append
   #:append!))