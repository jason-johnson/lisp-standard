(in-package #:std.collection.impl-common.unordered)

;; NOTE: We make the user pass every symbol we define because I don't know how else to inject the definition into the calling package

(defmacro define-collection-functions (name do reduce count count-if count-if-not find find-if find-if-not &optional (value 'value) (value-form 'value))
  (let ((val (gensym)))
    `(progn
       (defun ,reduce (function ,name &key key from-end (initial-value nil initial-value-p))
	 (let ((last-result initial-value)
	       (has-value initial-value-p)
	       (f (if from-end
		      (lambda (r v) (funcall function v r))
		      function)))
	   (,do (,value ,name last-result)
		(let* ((,val ,value-form)
		       (v (if key		; TODO: Would it be faster to set key to id by default and always call it?
			     (funcall key ,val)
			     ,val)))
		  (setf last-result
			(if has-value
			    (funcall f last-result v)
			    v)
			has-value t)))))
       (defun ,count-if (predicate ,name &optional key)
	 (let ((p (if key
		      (compose predicate key)
		      predicate))
	       (count 0))
	   (,do (,value ,name count)
		(when (funcall p ,value-form)
		  (incf count)))))

       (defun ,count-if-not (predicate ,name &optional key)
	 (,count-if (compose #'not predicate) ,name key))

       (defun ,count (item ,name &key key test test-not)
	 (cond
	   (test (,count-if (lambda (v) (funcall test v item)) ,name key))
	   (test-not (,count-if-not (lambda (v) (funcall test-not v item)) ,name key))
	   (t (,count-if (lambda (v) (eql v item)) ,name key))))

       (defun ,find-if (predicate ,name &optional key)
	 (let ((p (if key
		      (compose predicate key)
		      predicate)))
	   (,do (,value ,name)
		(let ((,val ,value-form))
		  (when (funcall p ,val)
		    (return-from ,find-if ,val))))))

       (defun ,find-if-not (predicate ,name &optional key)
	 (,find-if (compose #'not predicate) ,name key))

       (defun ,find (item ,name &key key test test-not)
	 (cond
	   (test (,find-if (lambda (v) (funcall test v item)) ,name key))
	   (test-not (,find-if-not (lambda (v) (funcall test-not v item)) ,name key))
	   (t (,find-if (lambda (v) (eql v item)) ,name key)))))))