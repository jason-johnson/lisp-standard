(in-package #:cl-user)

(defpackage #:standard.collection
  (:nicknames #:std.collection #:collection)
  (:use #:cl #:std.base)
  (:shadow #:get #:remove #:map)
  (:export
   #:get
   #:put!
   #:copy
   #:remove
   #:remove!
   #:map-to))