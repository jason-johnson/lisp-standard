(in-package #:std.collection.array)

;; Meta

;; TODO: This only needs to be defined in one of the array types and just imported/rexported by the others (same with copy above)
(defun-alias 'array-rank 'rank)
(defun-alias 'array-dimension 'dimension)
(defun-alias 'array-dimensions 'dimensions)
(defun-alias 'array-total-size 'total-size)
(defun-alias 'array-element-type 'element-type)
(defun-alias 'array-in-bounds-p 'in-bounds-p)
(defun-alias 'array-displacement 'displacement)
(defun-alias 'array-row-major-index 'row-major-index)
(defun-alias 'array-has-fill-pointer-p 'has-fill-pointer-p)
(defun-alias 'adjustable-array-p 'adjustable-p)

;; Array specific

(defun-alias 'row-major-aref 'row-major-get)

(defsetf row-major-get (array index) (value)
  `(setf (row-major-aref ,array ,index) ,value))

(defun row-major-put! (array index value)
  (setf (row-major-get array index) value))

;;  Normal access

(defun-alias 'make-array 'make)

(declaim (inline get put!))

(defun get (array subscripts)
  (apply #'aref array subscripts))

(defsetf get (array subscripts) (value)
  `(setf (apply #'aref ,array ,subscripts) ,value))

(defun put! (array subscripts value)
  (setf (get array subscripts) value))

(defun copy (array)
  (flet ((copy-arguments (array)
	   (let ((args (list (dimensions array))))
	     (unless (eq (element-type array) t)
	       (push :element-type args)
	       (push (element-type array) args))
	     (when (eq (adjustable-p array) t)
	       (push :adjustable args)
	       (push t args))
	     (when (and
		    (vectorp array)
		    (has-fill-pointer-p array))
	       (push :fill-pointer args)
	       (push (fill-pointer array) args))
	     (when (displacement array)
	       (error "copy not supported for displaced arrays"))
	     (nreverse args))))
    (let ((result (apply 'make (copy-arguments array))))
      (dotimes (i (total-size array))
	(setf (row-major-get result i) (row-major-get array i)))
      result)))

(defmacro do ((var array &optional result) &body body)
  `(loop for ,var across ,array
	do (progn
	     ,@body)
	finally (return ,result)))

;; Generic methods

(defmethod std.collection:get ((container array) index)
  (get container index))

(defmethod std.collection:put! ((container array) index value)
  (setf (get container index) value))

(defmethod std.collection:get ((container vector) index) ; NOTE: Maybe more efficient (doesn't need apply)
  (aref container index))

(defmethod std.collection:put! ((container vector) index value)
  (setf (aref container index) value))

(defmethod std.base:copy ((object array))
  (copy object))