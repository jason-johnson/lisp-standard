(in-package #:cl-user)

(defpackage #:standard.base
  (:nicknames #:std.base)
  (:use #:cl)
  (:export
   #:defun-alias
   #:def-metapackage
   #:def-metapackage-compliment
   #:set!
   #:setf					; For people who prefer this name
   #:inc!
   #:dec!
   #:compose
   #:copy))
