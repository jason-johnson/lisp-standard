(in-package #:std.collection.impl-common)

;; Package to collect common operations used by most collections

;; Rename sequence functions according to the convention

(defun-alias 'delete 'remove^)
(defun-alias 'delete-if 'remove-if^)
(defun-alias 'delete-if-not 'remove-if-not^)
(defun-alias 'delete-duplicates 'remove-duplicates^)
(defun-alias 'nreverse 'reverse^)
(defun-alias 'nsubstitute 'substitute!)
(defun-alias 'nsubstitute-if 'substitute-if!)
(defun-alias 'nsubstitute-if-not 'substitute-if-not!)
(defun-alias 'nconc 'append!)
(defun-alias 'sort 'sort^)

;; Define sensible default for all collection generic functions

(defmethod std.collection:copy ((collection sequence) &optional (start 0) end)
  (subseq collection start end))

(defmethod std.collection:length ((collection sequence))
  (length collection))

(defmethod std.collection:position (item (collection sequence) &key from-end (start 0) end key test test-not)
  (position item collection :from-end from-end :start start :end end :key key :test test :test-not test-not))

(defmethod std.collection:position-if (predicate (collection sequence) &key from-end (start 0) end key)
  (position-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:position-if-not (predicate (collection sequence) &key from-end (start 0) end key)
  (position-if-not predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:count (item (collection sequence) &key from-end (start 0) end key (test #'eql) (test-not nil test-not-p))
  (apply #'count item collection :from-end from-end :start start :end end :key key
	 (if test-not-p
	     (list :test-not test-not)
	     (list :test test))))

(defmethod std.collection:count-if (predicate (collection sequence) &key from-end (start 0) end key)
  (count-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:count-if-not (predicate (collection sequence) &key from-end (start 0) end key)
  (count-if-not predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:reduce (function (collection sequence) &key key from-end (start 0) end (initial-value nil initial-value-p))
  (apply #'reduce function collection :key key :from-end from-end :start start :end end (if initial-value-p (list :initial-value initial-value))))

(defmethod std.collection:find (item (collection sequence) &key from-end (start 0) end key test test-not)
  (find item collection :from-end from-end :test test :test-not test-not :start start :end end :key key))

(defmethod std.collection:find-if (predicate (collection sequence) &key from-end (start 0) end key)
  (find-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:find-if-not (predicate (collection sequence) &key from-end (start 0) end key)
  (find-if-not predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:remove (item (collection sequence) &key from-end (test #'eql) test-not (start 0) end count key)
  (remove item collection :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:remove^ (item (collection sequence) &key from-end (test #'eql) test-not (start 0) end count key)
  (remove^ item collection :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:remove-if (predicate (collection sequence) &key from-end (start 0) end count key)
  (remove-if predicate collection :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-if^ (predicate (collection sequence) &key from-end (start 0) end count key)
  (remove-if^ predicate collection :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-if-not (predicate (collection sequence) &key from-end (start 0) end count key)
  (remove-if-not predicate collection :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-if-not^ (predicate (collection sequence) &key from-end (start 0) end count key)
  (remove-if-not^ predicate collection :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-duplicates ((collection sequence) &key (test #'eql) test-not (start 0) end from-end key)
  (remove-duplicates collection :from-end from-end :test test :test-not test-not :start start :end end :key key))

(defmethod std.collection:remove-duplicates^ ((collection sequence) &key (test #'eql) test-not (start 0) end from-end key)
  (remove-duplicates^ collection :from-end from-end :test test :test-not test-not :start start :end end :key key))

(defmethod std.collection:reverse ((collection sequence))
  (reverse collection))

(defmethod std.collection:reverse^ ((collection sequence))
  (reverse^ collection))

(defmethod std.collection:fill ((collection sequence) item &key (start 0) end)
  (fill collection item :start start :end end))

(defmethod std.collection:substitute (new old (collection sequence) &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute new old collection :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:substitute! (new old (collection sequence) &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute! new old collection :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:substitute-if (new predicate (collection sequence) &key from-end (start 0) count end key)
  (substitute-if new predicate collection :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:substitute-if! (new predicate (collection sequence) &key from-end (start 0) count end key)
  (substitute-if! new predicate collection :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:substitute-if-not ((collection sequence) predicate new &key from-end (start 0) count end key)
  (substitute-if-not new predicate collection :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:substitute-if-not! (new predicate (collection sequence) &key from-end (start 0) count end key)
  (substitute-if-not! new predicate collection :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:append ((collection sequence) &rest collections)
  (apply #'append collection collections))

(defmethod std.collection:append! ((collection sequence) &rest collections)
  (apply #'append! collection collections))

(defmethod std.collection:sort^ ((collection sequence) predicate &key key)
  (sort collection predicate :key key))

(defmethod std.collection:stable-sort^ ((collection sequence) predicate &key key)
  (stable-sort collection predicate :key key))

(defmethod std.collection:merge^ (output-spec (collection1 sequence) (collection2 sequence) predicate &key key)
  (merge output-spec collection1 collection2 predicate :key key))

(defmethod std.collection:merge (output-spec (collection1 sequence) (collection2 sequence) predicate &key key)
  (std.collection:merge^ output-spec (copy-seq collection1) (copy-seq collection2) predicate :key key))