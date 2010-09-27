(in-package #:std.collection.hash.values)

(defmacro do ((value hash &optional result) &body body) ;TODO: Needs a with-gensyms on hash
  "Iterate over a hash's values"
  `(hash:do ((nil ,value) ,hash ,result)
	    ,@body))

(impl-common.unordered:define-collection-functions hash do reduce count count-if count-if-not find find-if find-if-not)