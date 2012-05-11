(in-package #:std.collection.list)

(defmethod std.collection:get ((collection cons) index)
  (get collection index))

(defmethod std.collection:put! ((collection cons) index value)
  (setf (get collection index) value))

(defmethod std.base:copy ((object cons))
  (copy object))

;; NOTE: Use common collection:copy

(defmethod std.collection:copy^ ((collection cons) &optional (start 0) end)
  (copy^ collection start end))

(defmethod std.collection:sort ((collection cons) predicate &key key)
  (sort collection predicate :key key))

(defmethod std.collection:stable-sort ((collection cons) predicate &key key)
  (stable-sort collection predicate :key key))

(defmethod std.collection:merge ((output-spec (eql 'list)) (collection1 cons) (collection2 cons) predicate &key key)
  (merge collection1 collection2  predicate :key key))

(defmethod std.collection:split (delimiter (collection list) &key key from-end (start 0) end count (test #'eql) remove-empty)
  (split delimiter collection :key key :from-end from-end :start start :end end :count count :test test :remove-empty remove-empty))

(defmethod std.collection:split-if (predicate (collection list) &key key from-end (start 0) end count remove-empty)
  (split-if predicate collection :key key :from-end from-end :start start :end end :count count :remove-empty remove-empty))