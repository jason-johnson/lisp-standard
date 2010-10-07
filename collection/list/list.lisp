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