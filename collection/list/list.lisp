(in-package #:std.collection.list)

;; List specific

(defun-alias 'mapcan 'map-flatten)
(defun-alias 'mapcon 'maplist-flatten)
(defun-alias 'mapc 'foreach)
(defun-alias 'mapl 'foreach-list)

(defun-alias 'nbutlast 'butlast^)
(defun-alias 'revappend 'reverse-append)
(defun-alias 'nreconc 'reverse-append^)

(defmacro push! (obj list)
  `(push ,obj ,list))

(defmacro pop! (list)
  `(pop ,list))

;;  Normal access

(defun-alias 'make-list 'make)

(declaim (inline get put! sort stable-sort merge^ merge))

(defun get (list index)
  (nth index list))

(defsetf get (list index) (value)
  `(progn (rplaca (nthcdr ,index ,list) ,value)
	  ,value))

(defun put! (list index value)
  (setf (get list index) value))

(defun-alias 'copy-list 'copy)

(defun-alias 'copy-tree 'deep-copy)

(defun copy^ (list &optional (start 0) end)
  (let* ((count 0)
	 (head (cl:do ((head list (rest head)))
		      ((= start count)
		       head)
		 (incf count))))
    (when end
      (let* ((end (1- end))
	     (tail (cl:do ((tail head (rest tail)))
			  ((= end count)
			   tail)
		     (incf count))))
	(setf (rest tail) nil)))
    head))

(defun-alias 'list-length 'length)

(defun-alias 'nconc 'append!)

(defmacro do ((var list &optional result) &body body)
  `(dolist (,var ,list ,result)
     ,@body))

(defun-alias 'mapcar 'map)

(defun concatenate (&rest sequences)
  (apply #'cl:concatenate 'list sequences))

(defun sort (list predicate &key key)
  (cl:sort (deep-copy list) predicate :key key))

(defun stable-sort (list predicate &key key)
  (cl:stable-sort (deep-copy list) predicate :key key))

(defun merge^ (list1 list2 predicate &key key)
  (cl:merge 'list list1 list2 predicate :key key))

(defun merge (list1 list2 predicate &key key)
  (merge^ (deep-copy list1) (deep-copy list2) predicate :key key))

(defun split-if^ (predicate list &key key from-end (start 0) end count remove-empty)
  (let ((pushed 0)
	(first? t)
	(p (if key
	       (compose predicate key)
	       predicate))
	last
	result)
    (when end (incf end))
    (flet ((push-last ()
	     (unless (and remove-empty (not last))
	       (incf pushed)
	       (push last result))))
      (cl:do ((head list (rest head))
	      (pointer (cons nil list) head)
	      (current 0 (1+ current)))
	     ((or
	       (not head)
	       (and count (not from-end) (= count pushed)))
	      (progn
		(push-last)
		(when from-end
		  (setf result (subseq result 0 count)))
		(nreverse result)))
	(cond
	  ((< current start))
	  ((and end (= current end))
	   (rplacd pointer nil)
	   (setf head nil))
	  ((funcall p (first head))
	   (rplacd pointer nil)
	   (push-last)
	   (setf last nil
		 first? t))
	  (first?
	   (setf first? nil
		 last head)))))))

(defun split-if-not^ (predicate list &key key from-end (start 0) end count remove-empty)
  (split-if^ (complement predicate) list :key key :from-end from-end :start start :end end :count count :remove-empty remove-empty))

(defun split^ (delimiter list &key key from-end (start 0) end count (equal 'eql) test test-not remove-empty)
  (cond
    (test (split-if^ (lambda (v) (funcall test v delimiter)) list :key key :from-end from-end :start start :end end :count count :remove-empty remove-empty))
    (test-not (split-if-not^ (lambda (v) (funcall test-not v delimiter)) list :key key :from-end from-end :start start :end end :count count :remove-empty remove-empty))
    (t (split-if^ (lambda (v) (funcall equal v delimiter)) list :key key :from-end from-end :start start :end end :count count :remove-empty remove-empty))))

(defun split-if (predicate list &key key from-end (start 0) end count remove-empty)
  (split-if^ predicate (copy list) :key key :from-end from-end :start start :end end :count count :remove-empty remove-empty))

(defun split-if-not (predicate list &key key from-end (start 0) end count remove-empty)
  (split-if-not^ predicate (copy list) :key key :from-end from-end :start start :end end :count count :remove-empty remove-empty))

(defun split (delimiter list &key key from-end (start 0) end count (equal 'eql) test test-not remove-empty)
  (split^ delimiter (copy list) :key key :from-end from-end :start start :end end :count count :equal equal :test test :test-not test-not :remove-empty remove-empty))