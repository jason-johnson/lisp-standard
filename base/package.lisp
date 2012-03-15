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
   #:compose
   #:iterate
   #:with-gensyms
   #:once-only
   #:copy
   #:coerce))
