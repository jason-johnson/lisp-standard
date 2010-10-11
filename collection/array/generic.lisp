(in-package #:std.collection.array)

;; Generic methods

(defmethod std.collection:get ((collection array) indexes)
  (apply #'get collection indexes))

(defmethod std.collection:put! ((collection array) indexes value)
  (let ((i (apply #'row-major-index collection indexes)))
    (row-major-put! collection i value)))

(defmethod std.base:copy ((object array))
  (copy object))

(defmethod std.collection:length ((collection array))
  (length collection))

(defmethod std.collection:count (item (collection array) &key from-end (start 0) end key test test-not)
  (count item collection :from-end from-end :start start :end end :key key :test test :test-not test-not))

(defmethod std.collection:count-if (predicate (collection array) &key from-end (start 0) end key)
  (count-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:count-if-not (predicate (collection array) &key from-end (start 0) end key)
  (count-if-not predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:reduce (function (collection array) &key key from-end (start 0) end (initial-value nil initial-value-p))
  (apply #'reduce function collection :key key :from-end from-end :start start :end end (if initial-value-p (list :initial-value initial-value))))

(defmethod std.collection:find (item (collection array) &key from-end (start 0) end key test test-not)
  (find item collection :from-end from-end :start start :end end :key key :test test :test-not test-not))

(defmethod std.collection:find-if (predicate (collection array) &key from-end (start 0) end key)
  (find-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:find-if-not (predicate (collection array) &key from-end (start 0) end key)
  (find-if-not predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:sort ((collection array) predicate &key key)
  (sort (copy collection) predicate :key key))