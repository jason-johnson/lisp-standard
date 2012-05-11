(in-package #:std.collection.hash)

(defmethod std.collection:get ((collection hash-table) key)
  (get collection key))

(defmethod std.collection:put! ((collection hash-table) key value)
  (put! collection key value))

(defmethod std.base:copy ((collection hash-table))
  (copy collection))

(defmethod std.collection:length ((collection hash-table))
  (length collection))

(defmethod std.collection:reduce (function (collection hash-table) &key key from-end start end (initial-value nil initial-value-p))
  (declare (ignore start end))
  (apply #'hash.values:reduce function collection :key key :from-end from-end (if initial-value-p (list :initial-value initial-value))))

(defmethod std.collection:find (item (collection hash-table) &key from-end start end key (test #'eql))
  (declare (ignore from-end start end))
  (hash.values:find item collection :key key :test test))

(defmethod std.collection:find-if (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (hash.values:find-if predicate collection key))

(defmethod std.collection:count (item (collection hash-table) &key from-end start end key (test #'eql))
  (declare (ignore from-end start end))
  (hash.values:count item collection :key key :test test))

(defmethod std.collection:count-if (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (hash.values:count-if predicate collection key))