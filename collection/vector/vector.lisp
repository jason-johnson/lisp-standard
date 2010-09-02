(in-package #:std.collection.vector)

;; TODO: We might give this a different name, e.g. buffer, since it is a special, fixed size entity

;; Normal access

(declaim (inline get put!))

; NOTE: what we call "vector", common lisp calls "simple-vector", so we must restrict make so that it can't return anything we can't use
(defun make (dimension &key (element-type t) initial-element initial-contents)
  "Make a simple, fixed size vector"
  (flet ((compute-arguments ()
	   (let ((result (list dimension)))
	     (push :element-type result)
	     (push element-type result)
	     (when initial-contents
	       (push :initial-contents result)
	       (push initial-contents result))
	     (when initial-element
	       (push :initial-element result)
	       (push initial-element result))
	     (nreverse result))))
    (apply #'make-array (compute-arguments))))

(defun get (vector index)
  (svref vector index))

(defsetf get (vector index) (value)
  `(setf (svref ,vector ,index) ,value))

(defun put! (vector index value)
  (setf (get vector index) value))

;; Generic access

(defmethod std.collection:get ((container simple-vector) index)
  (get container index))

(defmethod std.collection:put! ((container simple-vector) index value)
  (setf (get container index) value))

;; NOTE: vector and simple-vector are subtypes of array so the copy defined in the array module works for us too
