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

(defmethod std.collection:count (item (collection array) &key from-end (start 0) end key (test #'eql))
  (count item collection :from-end from-end :start start :end end :key key :test test))

(defmethod std.collection:count-if (predicate (collection array) &key from-end (start 0) end key)
  (count-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:reduce (function (collection array) &key key from-end (start 0) end (initial-value nil initial-value-p))
  (apply #'reduce function collection :key key :from-end from-end :start start :end end (if initial-value-p (list :initial-value initial-value))))

(defmethod std.collection:find (item (collection array) &key from-end (start 0) end key (test #'eql))
  (find item collection :from-end from-end :start start :end end :key key :test test))

(defmethod std.collection:find-if (predicate (collection array) &key from-end (start 0) end key)
  (find-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:sort ((collection array) predicate &key key)
  (sort (copy collection) predicate :key key))

(defmethod std.collection:position-if (predicate (collection array) &key from-end start end key)
  (position-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:position (item (collection array) &key from-end start end key (test #'eql))
  (position item collection :from-end from-end :start start :end end :key key :test test))

(defmethod std.collection:substitute-if (new predicate (collection array) &key from-end (start 0) end key count)
  (substitute-if new predicate collection :from-end from-end :start start :end end :key key :count count))

(defmethod std.collection:substitute (new old (collection array) &key from-end (start 0) end key count (test #'eql))
  (substitute new old collection :from-end from-end :start start :end end :key key :count count :test test))

(defmethod std.collection:substitute-if! (new predicate (collection array) &key from-end (start 0) end key count)
  (substitute-if! new predicate collection :from-end from-end :start start :end end :key key :count count))

(defmethod std.collection:substitute! (new old (collection array) &key from-end (start 0) end key count (test #'eql))
  (substitute! new old collection :from-end from-end :start start :end end :key key :count count :test test))

(defmethod std.collection:reverse ((collection array))
  (reverse collection))

(defmethod std.collection:reverse^ ((collection array))
  (reverse! collection))