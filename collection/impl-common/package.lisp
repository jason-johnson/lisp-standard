(in-package #:cl-user)

(defpackage #:standard.collection.impl-common
  (:nicknames #:std.collection.impl-common #:impl-common)
  (:use #:cl)
  (:import-from #:std.base #:defun-alias)
  (:export
   #:remove
   #:remove^
   #:remove-if
   #:remove-if^
   #:filter
   #:filter^
   #:remove-duplicates
   #:remove-duplicates^
   #:reverse
   #:reverse^
   #:substitute
   #:substitute!
   #:substitute-if
   #:substitute-if!
   #:append
   #:append!
   #:sort^))