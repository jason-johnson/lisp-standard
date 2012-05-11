(in-package #:std.collection.impl-common.unordered)

;; NOTE: We make the user pass every symbol we define because I don't know how else to inject the definition into the calling package

(defmacro define-collection-functions (name do &key (value 'value) (value-form 'value) reduce count count-if find find-if)
  (let ((val (gensym)))
    `(progn
       ,@(if reduce
	     `((defun ,reduce (function ,name &key key from-end (initial-value nil initial-value-p))
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
			       has-value t)))))))

       ,@(if count-if
	     `((defun ,count-if (predicate ,name &optional key)
		(let ((p (if key
			     (compose predicate key)
			     predicate))
		      (count 0))
		  (,do (,value ,name count)
		       (when (funcall p ,value-form)
			 (incf count)))))))

       ,@(if (and count-if count)
	     `((defun ,count (item ,name &key key (test #'eql))
		 (,count-if (lambda (v) (funcall test v item)) ,name key))))

       ,@(if find-if
	     `((defun ,find-if (predicate ,name &optional key)
		(let ((p (if key
			     (compose predicate key)
			     predicate)))
		  (,do (,value ,name)
		       (let ((,val ,value-form))
			 (when (funcall p ,val)
			   (return-from ,find-if ,val))))))))

       ,@(if (and find-if find)
	     `((defun ,find (item ,name &key key (test #'eql))
		 (,find-if (lambda (v) (funcall test v item)) ,name key)))))))