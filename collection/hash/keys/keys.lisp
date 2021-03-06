(in-package #:std.collection.hash.keys)

(defmacro do ((key hash &optional result) &body body) ;TODO: Needs a with-gensyms on hash
  "Iterate over a hash's values"
  `(hash:do ((,key nil) ,hash ,result)
	    ,@body))

(impl-common.unordered:define-collection-functions hash do reduce count count-if count-if-not find find-if find-if-not)