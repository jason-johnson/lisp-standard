(in-package #:std.collection.string)

;; Normal access

(defun make (dimension &key (element-type 'character) initial-element adjustable fill-pointer displaced-to displaced-index-offset)
  "Make a string"
  (flet ((compute-arguments ()
	   (let ((result (list dimension)))
	     (flet ((maybe-put! (key value)
		      (when value
			(push key result)
			(push value result))))
	       (maybe-put! :element-type element-type)
	       (maybe-put! :initial-element initial-element)
	       (maybe-put! :adjustable adjustable)
	       (maybe-put! :fill-pointer fill-pointer)
	       (maybe-put! :displaced-to displaced-to)
	       (maybe-put! :displaced-index-offset displaced-index-offset))
	     (nreverse result))))
    (apply #'make-array (compute-arguments))))

;; Generic access

;; NOTE: vector and simple-vector are subtypes of array so the copy defined in the array module works for us too
