(in-package #:std.collection.bit)

;; Normal access

(defun make (dimension &key initial-contents initial-element adjustable fill-pointer displaced-to displaced-index-offset)
  "Make a bit vector"
  (flet ((compute-arguments ()
	   (let ((result (list 'bit :element-type dimension)))
	     (flet ((maybe-put! (key value)
		      (when value
			(push key result)
			(push value result))))
	       (maybe-put! :initial-element initial-element)
	       (maybe-put! :initial-contents initial-contents)
	       (maybe-put! :adjustable adjustable)
	       (maybe-put! :fill-pointer fill-pointer)
	       (maybe-put! :displaced-to displaced-to)
	       (maybe-put! :displaced-index-offset displaced-index-offset))
	     (nreverse result))))
    (apply #'make-array (compute-arguments))))

(defun concatenate (&rest sequences)
  (apply #'cl:concatenate 'vector sequences))

;; Generic access

