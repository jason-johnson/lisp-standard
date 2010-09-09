(in-package #:std.collection.impl-common)

;; Package to collect common operations used by most collections

;; Rename sequence functions according to the convention

(defun-alias 'delete 'remove^)		; TODO: This should actually be remove^
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

(defmethod std.collection:reduce (function container &key key from-end (start 0) end initial-value)
  (reduce function container :key key :from-end from-end :start start :end end :initial-value initial-value))

(defmethod std.collection:find (item (container sequence) &key from-end (start 0) end key test test-not)
  (find item container :from-end from-end :test test :test-not test-not :start start :end end :key key))

(defmethod std.collection:find-if (predicate container &key from-end (start 0) end key)
  (find-if predicate container :from-end from-end :start start :end end :key key))

(defmethod std.collection:find-if-not (predicate container &key from-end (start 0) end key)
  (find-if-not predicate container :from-end from-end :start start :end end :key key))

(defmethod std.collection:remove (item (container sequence) &key from-end (test #'eql) test-not (start 0) end count key)
  (remove item container :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:remove^ (item (container sequence) &key from-end (test #'eql) test-not (start 0) end count key)
  (remove^ item container :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:remove-if (predicate (container sequence) &key from-end (start 0) end count key)
  (remove-if predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-if^ (predicate (container sequence) &key from-end (start 0) end count key)
  (remove-if^ predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-if-not (predicate (container sequence) &key from-end (start 0) end count key)
  (remove-if-not predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-if-not^ (predicate (container sequence) &key from-end (start 0) end count key)
  (remove-if-not^ predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-duplicates ((container sequence) &key (test #'eql) test-not (start 0) end from-end key)
  (remove-duplicates container :from-end from-end :test test :test-not test-not :start start :end end :key key))

(defmethod std.collection:remove-duplicates^ ((container sequence) &key (test #'eql) test-not (start 0) end from-end key)
  (remove-duplicates^ container :from-end from-end :test test :test-not test-not :start start :end end :key key))

(defmethod std.collection:reverse ((container sequence))
  (reverse container))

(defmethod std.collection:reverse^ ((container sequence))
  (reverse^ container))

(defmethod std.collection:substitute (new old (container sequence) &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute new old container :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:substitute! (new old (container sequence) &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute! new old container :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:substitute-if (new predicate (container sequence) &key from-end (start 0) count end key)
  (substitute-if new predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:substitute-if! (new predicate (container sequence) &key from-end (start 0) count end key)
  (substitute-if! new predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:substitute-if-not ((container sequence) predicate new &key from-end (start 0) count end key)
  (substitute-if-not new predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:substitute-if-not! (new predicate (container sequence) &key from-end (start 0) count end key)
  (substitute-if-not! new predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:append ((container sequence) &rest containers)
  (apply #'append container containers))

(defmethod std.collection:append! ((container sequence) &rest containers)
  (apply #'append! container containers))

(defmethod std.collection:sort^ ((container sequence) predicate &key key)
  (sort container predicate :key key))

(defmethod std.collection:stable-sort^ ((container sequence) predicate &key key)
  (stable-sort container predicate :key key))

(defmethod std.collection:merge^ (output-spec (container1 sequence) (container2 sequence) predicate &key key)
  (merge output-spec container1 container2 predicate :key key))

(defmethod std.collection:merge (output-spec (container1 sequence) (container2 sequence) predicate &key key)
  (std.collection:merge^ output-spec (copy-seq container1) (copy-seq container2) predicate :key key))
