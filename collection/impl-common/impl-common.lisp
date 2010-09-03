(in-package #:std.collection.impl-common)

;; Package to collect common operations used by most collections

;; Rename sequence functions according to the convention

(defun-alias 'delete 'remove!)
(defun-alias 'delete-if 'remove-if!)
(defun-alias 'delete-if-not 'remove-if-not!)
(defun-alias 'delete-duplicates 'remove-duplicates!)
(defun-alias 'nreverse 'reverse!)
(defun-alias 'nsubstitute 'substitute!)
(defun-alias 'nsubstitute-if 'substitute-if!)
(defun-alias 'nsubstitute-if-not 'substitute-if-not!)
(defun-alias 'nconc 'append!)

;; Define sensible default for all collection generic functions

(defmethod std.collection:remove ((container sequence) item &key from-end (test #'eql) test-not (start 0) end count key)
  (remove item container :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:remove! ((container sequence) item &key from-end (test #'eql) test-not (start 0) end count key)
  (remove! item container :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:remove-if (predicate (container sequence) &key from-end (start 0) end count key)
  (remove-if predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-if! (predicate (container sequence) &key from-end (start 0) end count key)
  (remove-if! predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-if-not (predicate (container sequence) &key from-end (start 0) end count key)
  (remove-if-not predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-if-not! (predicate (container sequence) &key from-end (start 0) end count key)
  (remove-if-not! predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:remove-duplicates ((container sequence) &key (test #'eql) test-not (start 0) end from-end key)
  (remove-duplicates container :from-end from-end :test test :test-not test-not :start start :end end :key key))

(defmethod std.collection:remove-duplicates! ((container sequence) &key (test #'eql) test-not (start 0) end from-end key)
  (remove-duplicates! container :from-end from-end :test test :test-not test-not :start start :end end :key key))

(defmethod std.collection:reverse ((container sequence))
  (reverse container))

(defmethod std.collection:reverse! ((container sequence))
  (reverse! container))

(defmethod std.collection:substitute ((container sequence) old new &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute new old container :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:substitute! ((container sequence) old new &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute! new old container :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:substitute-if ((container sequence) predicate new &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute-if new predicate container :from-end from-end :test test :test-not test-not :start start :end end :count count :key key))

(defmethod std.collection:substitute-if! ((container sequence) predicate new &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute-if! new predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:substitute-if-not ((container sequence) predicate new &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute-if-not new predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:substitute-if-not! ((container sequence) predicate new &key from-end (test #'eql) test-not (start 0) count end key)
  (substitute-if-not! new predicate container :from-end from-end :start start :end end :count count :key key))

(defmethod std.collection:append ((container sequence) &rest containers)
  (apply #'append container containers))

(defmethod std.collection:append! ((container sequence) &rest containers)
  (apply #'append! container containers))