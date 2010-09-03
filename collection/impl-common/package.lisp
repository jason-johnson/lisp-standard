(in-package #:cl-user)

(defpackage #:standard.collection.impl-common
  (:nicknames #:std.collection.impl-common)
  (:use #:cl)
  (:import-from #:std.base #:defun-alias)
  (:export
   #:remove
   #:remove^
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
