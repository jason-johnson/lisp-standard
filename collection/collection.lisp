(in-package #:std.collection)

(defgeneric get (container key)
  (:documentation "Get value for key"))

(defgeneric put! (container key value)
  (:documentation "Put value at key"))

(defgeneric remove (container item &key from-end test test-not start end count key)
  (:documentation "Remove item from collection"))

(defgeneric remove! (container item &key from-end test test-not start end count key)
  (:documentation "Destructive form of remove"))

(defgeneric remove-if (predicate container &key from-end (start 0) end count key)
  (:documentation "Remove items from container for which predicate returns t"))

(defgeneric remove-if! (predicate container &key from-end (start 0) end count key)
  (:documentation "Destructive form of remove-if"))

(defgeneric remove-if-not (predicate container &key from-end (start 0) end count key)
  (:documentation "Remove items from container for which predicate returns nil"))

(defgeneric remove-if-not! (predicate container &key from-end (start 0) end count key)
  (:documentation "Destructive form of remove-if-not"))

(defgeneric remove-duplicates (container &key (test #'eql) test-not (start 0) end from-end key)
  (:documentation "Remove duplicates from container"))

(defgeneric remove-duplicates! (container &key (test #'eql) test-not (start 0) end from-end key)
  (:documentation "Destructive form of remove-duplicates"))

(defgeneric reverse (container)
  (:documentation "Reverse the contents of a container"))

(defgeneric reverse! (container)
  (:documentation "Reverse the contents of a container in place (destructive)"))

(defgeneric substitute (container old new &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute! (container old new &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute-if (container predicate new &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute-if! (container predicate new &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute-if-not (container predicate new &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute-if-not! (container predicate new &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric append (container &rest containers)
  (:documentation "Append containers together with container"))

(defgeneric append! (container &rest containers)
  (:documentation "Destructive form of append"))
