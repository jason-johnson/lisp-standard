(in-package #:std.collection.hash.keys)

(defmacro do ((key hash &optional result) &body body)
  "Iterate over a hash's keys"
  `(hash:do ((,key nil) ,hash ,result)
	    ,@body))

(impl-common.unordered:define-collection-functions hash do :reduce reduce :count count :count-if count-if :find find :find-if find-if)