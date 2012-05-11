(in-package #:std.collection.hash.values)

(defmacro do ((value hash &optional result) &body body)
  "Iterate over a hash's values"
  `(hash:do ((nil ,value) ,hash ,result)
	    ,@body))

(impl-common.unordered:define-collection-functions hash do :reduce reduce :count count :count-if count-if :find find :find-if find-if)