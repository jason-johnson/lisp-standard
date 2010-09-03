(in-package #:std.collection.list)

;; List specific

(defun-alias 'mapcan 'map!)
(defun-alias 'mapcon 'maplist!)
(defun-alias 'mapc 'foreach)
(defun-alias 'mapl 'foreach-list)

(defun-alias 'nbutlast 'butlast^)
(defun-alias 'revappend 'reverse-append)
(defun-alias 'nreconc 'reverse-append^)

;;  Normal access

(defun-alias 'make-list 'make)

(declaim (inline get put! sort))

(defun get (list index)
  (nth index list))

(defsetf get (list index) (value)
  `(progn (rplaca (nthcdr ,index ,list) ,value)
	  ,value))

(defun put! (list index value)
  (setf (get list index) value))

(defun-alias 'copy-list 'copy)

(defun-alias 'copy-tree 'deep-copy)

(defmacro do ((var list &optional result) &body body)
  `(dolist (,var ,list ,result)
     ,@body))

(defun-alias 'mapcar 'map)

(defun sort (list predicate &key key)
  (cl:sort (deep-copy list) predicate :key key))

;; Generic methods

(defmethod std.collection:get ((container cons) index)
  (get container index))

(defmethod std.collection:put! ((container cons) index value)
  (setf (get container index) value))

(defmethod std.base:copy ((object cons))
  (copy-list object))

(defmethod std.collection:sort ((container cons) predicate &key key)
  (sort container predicate :key key))
