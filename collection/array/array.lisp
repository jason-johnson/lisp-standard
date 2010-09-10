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

(defun-alias 'adjust-array 'adjust)

;; Array specific

(defun-alias 'row-major-aref 'row-major-get)

(defsetf row-major-get (array index) (value)
  `(setf (row-major-aref ,array ,index) ,value))

(defun row-major-put! (array index value)
  (setf (row-major-get array index) value))

;;  Normal access

(defun-alias 'make-array 'make)

(declaim (inline get length find-if-not find))

(defun get (array &rest subscripts)
  (apply #'aref array subscripts))

(defsetf get (array &rest subscripts) (value)
  `(setf (aref ,array ,@subscripts) ,value))

; NOTE: We don't define a put! since we can't sensibly define one with &rest subscripts

(defun length (array)			; TODO: Is this what we want?  We already have total-size for total size, but this is different than everything else we do with arrays
  (first (dimensions array)))

(defun find-if (predicate array &key from-end (start 0) end key)
  (unless end (setf end (1- (total-size array))))
  (let ((p (if key
	       (compose predicate key)
	       predicate))
	s e step check)
    (if from-end
	(setf s end e start step #'1- check #'<=)
	(setf s start e end step #'1+ check #'>=))
    (cl:do* ((i s (funcall step i))
	     (item (row-major-get array i) (row-major-get array i)))
	    ((funcall check i e))
      (if (funcall p item)
	  (return-from find-if item)))))

(defun find-if-not (predicate array &key from-end (start 0) end key)
  (find-if (compose #'not predicate) array :from-end from-end :start start :end end :key key))

(defun find (item array &key from-end (start 0) end key test test-not)
  (cond
    (test (find-if (lambda (v) (funcall test v item)) array :from-end from-end :start start :end end :key key))
    (test-not (find-if-not (lambda (v) (funcall test-not v item)) array :from-end from-end :start start :end end :key key))
    (t (find-if (lambda (v) (eql v item)) array :from-end from-end :start start :end end :key key))))

(defun copy (array)
  (flet ((copy-arguments (array)
	   (let ((args (list (dimensions array))))
	     (macrolet ((when-put! (test key value)
			  `(when ,test
			     (push ,key args)
			     (push ,value args))))
	       (when-put! (not (eq (element-type array) t)) :element-type (element-type array))
	       (when-put! (eq (adjustable-p array) t) :adjustable t)
	       (when-put! (and
			    (vectorp array)
			    (has-fill-pointer-p array))
			   :fill-pointer
			   (fill-pointer array)))
	     (when (displacement array)
	       (error "copy not supported for displaced arrays"))
	     (nreverse args))))
    (let ((result (apply 'make (copy-arguments array))))
      (dotimes (i (total-size array))
	(setf (row-major-get result i) (row-major-get array i)))
      result)))

(defmacro do ((var array &optional result) &body body)
  `(loop for i from 0 below (total-size ,array)
	do (let ((,var (row-major-get ,array i)))
	     ,@body)
	finally (return ,result)))

;; Generic methods

(defmethod std.collection:get ((container array) index)
  (get container index))

; NOTE: We don't define a collection:put! because I don't know how to make (put! *array* '(0 0 0) 'value) turn into (setf (get *array* 0 0 0) 'value).  We can't use apply because setf is a macro

(defmethod std.base:copy ((object array))
  (copy object))

(defmethod std.collection:sort ((container array) predicate &key key)
  (sort (copy container) predicate :key key))