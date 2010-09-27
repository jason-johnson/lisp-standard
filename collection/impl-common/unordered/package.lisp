(in-package #:cl-user)

(defpackage #:standard.collection.impl-common.unordered
  (:nicknames #:std.collection.impl-common.unordered #:impl-common.unordered)
  (:use #:cl)
  (:import-from #:std.base #:defun-alias #:compose)
  (:shadow #:count)
  (:export
   #:define-collection-functions))