(in-package #:cl-user)

(defpackage #:standard.base
  (:nicknames #:std.base)
  (:use #:cl)
  (:shadow #:coerce)
  (:export
   #:defun-alias
   #:def-metapackage
   #:def-metapackage-compliment
   #:set!
   #:inc!
   #:dec!
   #:rotate!
   #:while
   #:until
   #:compose
   #:iterate
   #:arr
   #:hash
   #:with-unique-names
   #:once-only
   #:copy
   #:coerce))
