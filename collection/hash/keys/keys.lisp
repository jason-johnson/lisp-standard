(in-package #:std.collection.hash.keys)

(defmacro do ((key hash &optional result) &body body) ;TODO: Needs a with-gensyms on hash
  "Iterate over a hash's values"
  `(hash:do ((,key nil) ,hash ,result)
	    ,@body))

(defun reduce (function hash &key key from-end (initial-value nil initial-value-p))
  (let ((last-result initial-value)
	(has-value initial-value-p)
	(f (if from-end
	       (lambda (r v) (funcall function v r))
	       function)))
    (do (value hash last-result)
	(let ((v (if key		; TODO: Would it be faster to set key to id by default and always call it?
		     (funcall key value)
		     value)))
	      (setf last-result
		    (if has-value
			(funcall f last-result v)
			v)
		    has-value t)))))