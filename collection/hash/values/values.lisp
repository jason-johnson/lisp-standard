(in-package #:std.collection.hash.values)

(defmacro do ((value hash &optional result) &body body)
  "Iterate over a hash's values"
  `(hash:do ((nil ,value) ,hash ,result)
	    ,@body))

(impl-common.unordered:define-collection-functions hash do :reduce reduce :count count :count-if count-if :find find :find-if find-if)

(defun position-if (predicate hash &optional key)
  (hash:do ((k v) hash nil)
	   (when (funcall predicate (if key (funcall key v) v))
	     (return-from position-if k))))

(defun position (item hash &key key (test #'eql))
  (position-if (lambda (e) (funcall test e item)) hash key))