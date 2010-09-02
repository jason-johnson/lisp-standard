(in-package #:std.collection)

;;  Normal access

(declaim (inline lref))

(defun lref (list n)
  (nth n list))

(defsetf lref (list n) (v)
  `(progn (rplaca (nthcdr ,n ,list) ,v)
	  ,v))

;; Generic access

(defmethod get ((container cons) index)
  (lref container index))

(defmethod set ((container cons) index value)
  (setf (lref container index) value))

;; NOTE: remove! can't be implemented for lists as a method

;; TODO:  Does this really make sense?  The existing remove method already does the right thing.
;; TODO:  This needs to be considered from the point of view of how lisp is used.  For remove we want copying, non-consing and I think we also want copy-in-place if that's possible (but it might not be)
(defmethod remove ((container cons) entry)
  (cl:remove entry container))

(defmethod copy ((container cons))
  (copy-list container))