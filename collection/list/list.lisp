(in-package #:std.collection.list)

;;  Normal access

(defun-alias 'make-list 'make)

(declaim (inline get put!))

(defun get (list index)
  (nth index list))

(defsetf get (list index) (value)
  `(progn (rplaca (nthcdr ,index ,list) ,value)
	  ,value))

(defun put! (list index value)
  (setf (get list index) value))

(defun-alias 'copy-list 'copy)


(defmacro do ((var list &optional result) &body body)
  `(dolist (,var ,list ,result)
     ,@body))

;; Generic methods

(defmethod std.collection:get ((container cons) index)
  (get container index))

(defmethod std.collection:put! ((container cons) index value)
  (setf (get container index) value))

(defmethod std.base:copy ((object cons))
  (copy-list object))