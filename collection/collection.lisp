(in-package #:std.collection)

(defgeneric get (container key)
  (:documentation "Get value for key"))

(defgeneric put! (container key value)
  (:documentation "Put value at key"))

;; TODO: I will probably want to create my own defgeneric that allows the defaults to be declared here, since this makes more sense.  If a method doesn't define any then
;; TODO: what was defined in the defgeneric is used, but defmethod always overrides if specified.  I could create my own generic function object to ensure that I have
;; TODO: have to do the minimal work.  That way the normal method detection (with the labmda list) works properly, and then I add the defaults to the method after the fact.

(defgeneric remove (item container &key from-end test test-not start end count key)
  (:documentation "Remove item from collection"))

(defgeneric remove! (item container &key from-end test test-not start end count key)
  (:documentation "Destructive form of remove"))

(defgeneric remove-if (predicate container &key from-end start end count key)
  (:documentation "Remove items from container for which predicate returns t"))

(defgeneric remove-if! (predicate container &key from-end start end count key)
  (:documentation "Destructive form of remove-if"))

(defgeneric remove-if-not (predicate container &key from-end start end count key)
  (:documentation "Remove items from container for which predicate returns nil"))

(defgeneric remove-if-not! (predicate container &key from-end start end count key)
  (:documentation "Destructive form of remove-if-not"))

(defgeneric remove-duplicates (container &key test test-not start end from-end key)
  (:documentation "Remove duplicates from container"))

(defgeneric remove-duplicates! (container &key test test-not start end from-end key)
  (:documentation "Destructive form of remove-duplicates"))

(defgeneric reverse (container)
  (:documentation "Reverse the contents of a container"))

(defgeneric reverse! (container)
  (:documentation "Reverse the contents of a container in place (destructive)"))

(defgeneric substitute (new old container &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute! (new old container &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute-if (new predicate container &key from-end start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute-if! (new predicate container &key from-end start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute-if-not (new predicate container &key from-end start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute-if-not! (new predicate container &key from-end start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric append (container &rest containers)
  (:documentation "Append containers together with container"))

(defgeneric append! (container &rest containers)
  (:documentation "Destructive form of append"))
