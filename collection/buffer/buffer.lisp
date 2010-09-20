(in-package #:std.collection.buffer)

;; Normal access

(declaim (inline get put!))

; NOTE: what we call "buffer", common lisp calls "simple-vector", so we must restrict make so that it can't return anything we can't use
(defun make (dimension &key (element-type t) initial-element initial-contents)
  "Make a simple, fixed size buffer"
  (flet ((compute-arguments ()
	   (let ((result (list dimension)))
	     (flet ((maybe-put! (key value)
		      (when value
			(push key result)
			(push value result))))
	       (maybe-put! :element-type element-type)
	       (maybe-put! :initial-contents initial-contents)
	       (maybe-put! :initial-element initial-element))
	     (nreverse result))))
    (apply #'make-array (compute-arguments))))

(defun get (buffer index)
  (svref buffer index))

(defsetf get (buffer index) (value)
  `(setf (svref ,buffer ,index) ,value))

(defun put! (buffer index value)
  (setf (get buffer index) value))

(defun map (fun &rest vectors)
  (apply #'map-to 'simple-vector fun (first vectors) (rest vectors)))

(defun concatenate (&rest sequences)
  (apply #'cl:concatenate 'simple-vector sequences))

;; Generic access

(defmethod std.collection:get ((collection simple-vector) index)
  (get collection index))

(defmethod std.collection:put! ((collection simple-vector) index value)
  (setf (get collection index) value))

;; NOTE: vector and simple-vector are subtypes of array so the copy defined in the array module works for us too
