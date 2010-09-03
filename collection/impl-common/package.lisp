(in-package #:cl-user)

(defpackage #:standard.collection.impl-common
  (:nicknames #:std.collection.impl-common)
  (:use #:std.collection #:cl)
  (:import-from #:std.base #:defun-alias)
  (:shadowing-import-from #:std.collection #:get)
  (:export
   #:remove
   #:remove!
   #:remove-if
   #:remove-if!
   #:remove-if-not
   #:remove-if-not!
   #:remove-duplicates
   #:remove-duplicates!
   #:reverse
   #:reverse!
   #:substitute
   #:substitute!
   #:substitute-if
   #:substitute-if!
   #:substitute-if-not
   #:substitute-if-not!
   #:append
   #:append!))

