(in-package #:std.collection.hash)

(defmethod std.collection:get ((collection hash-table) key)
  (get collection key))

(defmethod std.collection:put! ((collection hash-table) key value)
  (put! collection key value))

(defmethod std.base:copy ((collection hash-table))
  (copy collection))

(defmethod std.collection:reduce (function (collection hash-table) &key key from-end start end (initial-value nil initial-value-p))
  (declare (ignore start end))
  (apply #'reduce function collection :key key :from-end from-end (if initial-value-p (list :initial-value initial-value))))

(defmethod std.collection:find (item (collection hash-table) &key from-end start end key test test-not)
  (declare (ignore from-end start end))
  (find item collection :key key :test test :test-not test-not))

(defmethod std.collection:find-if (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (find-if predicate collection key))

(defmethod std.collection:find-if-not (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (find-if-not predicate collection key))

(defmethod std.collection:count (item (collection hash-table) &key from-end start end key test test-not)
  (declare (ignore from-end start end))
  (count item collection :key key :test test :test-not test-not))

(defmethod std.collection:count-if (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (count-if predicate collection key))

(defmethod std.collection:count-if-not (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (count-if-not predicate collection key))

;; NOTE: count ignored because it makes no sense.  If the user wants to remove all but one they can use remove-duplicates
;; (defmethod std.collection:remove^ (item (collection hash-table) &key from-end test test-not start end count key)
;;   (declare (ignore from-end start end count))
;;   (cond
;;     (test (remove-if^ (lambda (v) (funcall test v item)) collection key))
;;     (test-not (remove-if-not^ (lambda (v) (funcall test-not v item)) collection key))
;;     (t (remove-if^ (lambda (v) (eql v item)) collection key))))

;; (defmethod std.collection:remove (item (collection hash-table) &key from-end test test-not start end count key)
;;   (declare (ignore from-end start end count))
;;   (cond
;;     (test (remove-if (lambda (v) (funcall test v item)) collection key))
;;     (test-not (remove-if-not (lambda (v) (funcall test-not v item)) collection key))
;;     (t (remove-if (lambda (v) (eql v item)) collection key))))
