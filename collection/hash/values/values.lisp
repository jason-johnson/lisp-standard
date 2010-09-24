(in-package #:std.collection.hash.values)

(defmacro do ((value hash &optional result) &body body) ;TODO: Needs a with-gensyms on hash
  "Iterate over a hash's values"
  `(hash:do ((nil ,value) ,hash ,result)
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

(defun count-if (predicate hash &optional key)
  (let ((p (if key
	       (compose predicate key)
	       predicate))
	(count 0))
    (do (value hash count)
	(when (funcall p value)
	  (incf count)))))

(defun count-if-not (predicate hash &optional key)
  (count-if (compose #'not predicate) hash key))

(defun count (item hash &key key test test-not)
  (cond
    (test (count-if (lambda (v) (funcall test v item)) hash key))
    (test-not (count-if-not (lambda (v) (funcall test-not v item)) hash key))
    (t (count-if (lambda (v) (eql v item)) hash key))))

(defun find-if (predicate hash &optional key)
  (let ((p (if key
	       (compose predicate key)
	       predicate)))
    (do (value hash)
	(when (funcall p value)
	    (return-from find-if value)))))

(defun find-if-not (predicate hash &optional key)
  (find-if (compose #'not predicate) hash key))

(defun find (item hash &key key test test-not)
  (cond
    (test (find-if (lambda (v) (funcall test v item)) hash key))
    (test-not (find-if-not (lambda (v) (funcall test-not v item)) hash key))
    (t (find-if (lambda (v) (eql v item)) hash key))))