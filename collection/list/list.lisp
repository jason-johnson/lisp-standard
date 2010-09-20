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

(defun-alias 'list-length 'length)

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

;; Generic methods

(defmethod std.collection:get ((collection cons) index)
  (get collection index))

(defmethod std.collection:put! ((collection cons) index value)
  (setf (get collection index) value))

(defmethod std.base:copy ((object cons))
  (copy object))

(defmethod std.collection:sort ((collection cons) predicate &key key)
  (sort collection predicate :key key))

(defmethod std.collection:stable-sort ((collection cons) predicate &key key)
  (stable-sort collection predicate :key key))

(defmethod std.collection:merge ((output-spec (eql 'list)) (collection1 cons) (collection2 cons) predicate &key key)
  (merge collection1 collection2  predicate :key key))