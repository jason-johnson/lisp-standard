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
  (first (dimensions array)))		; TODO: I don't think so.  Since all functions use the total-size, length wouldn't be useful for anything

; TODO: This function needs some efficiency work
(defun map (function &rest arrays)
  (let* ((dimensions-list (apply #'mapcar #'min (mapcar #'dimensions arrays)))
	 (result (make dimensions-list))
	 (dimensions (coerce dimensions-list 'vector))
	 (indexes (make (cl:length dimensions) :initial-element 0))
	 (index (1- (cl:length dimensions))))
    (labels ((next (i)
	       (if (< i 0) (return-from map result))
	       (if (>= (get indexes i) (1- (get dimensions i)))
		   (progn
		     (setf (get indexes i) 0)
		     (next (1- i)))
		   (setf (get indexes i) (1+ (get indexes i)))))
	     (%get (array)
	       (let ((is (coerce indexes 'list))
		     (diff (- (list-length (dimensions array)) (cl:length indexes))))
		 (unless (eql diff 0)
		   (setf is (nreverse is))
		   (dotimes (dummy diff)
		     (cl:push 0 is))
		   (setf is (nreverse is)))
		 (apply #'get array is)))
	     (%map ()
	       (let ((i (apply #'row-major-index result (coerce indexes 'list)))
		     (e (apply function (mapcar #'%get arrays))))
		 (row-major-put! result i e))
	       (next index)
	       (%map)))
      (%map))))

(defun count-if (predicate array &key from-end (start 0) end key)
  (unless end (setf end (1- (total-size array))))
  (let ((count 0)
	(p (if key
	       (compose predicate key)
	       predicate))
	s e step check)
    (if from-end
	(setf
	 s end
	 e start
	 step #'1-
	 check #'<)
	(setf
	 s start
	 e end
	 step #'1+
	 check #'>))
    (cl:do* ((i s (funcall step i)))
	    ((funcall check i e) count)
      (when (funcall p (row-major-get array i))
	  (incf count)))))

(defun count-if-not (predicate array &key from-end (start 0) end key)
  (count-if (compose #'not predicate) array :from-end from-end :start start :end end :key key))

(defun count (item array &key from-end (start 0) end key test test-not)
  (cond
    (test (count-if (lambda (v) (funcall test v item)) array :from-end from-end :start start :end end :key key))
    (test-not (count-if-not (lambda (v) (funcall test-not v item)) array :from-end from-end :start start :end end :key key))
    (t (count-if (lambda (v) (eql v item)) array :from-end from-end :start start :end end :key key))))

(defun reduce (function array &key key from-end (start 0) end (initial-value nil initial-value-p))
  (unless end (setf end (1- (total-size array))))
  (let ((last-result initial-value)
	(get #'row-major-get)
	f s e step check)
    (if from-end
	(setf
	 f (lambda (r v) (funcall function v r))
	 s end
	 e start
	 step #'1-
	 check #'<=)
	(setf
	 f function
	 s start
	 e end
	 step #'1+
	 check #'>=))
    (if key
	(setf get (compose key get)))
    (unless initial-value-p
      (setf
       last-result (funcall get array s)
       s (funcall step s)))
    (cl:do* ((i s (funcall step i))
	     (value (funcall get array i) (funcall get array i))
	     (result (funcall f last-result value) (funcall f result value)))
	    ((funcall check i e) result))))

(defun find-if (predicate array &key from-end (start 0) end key)
  (unless end (setf end (1- (total-size array))))
  (let ((p (if key
	       (compose predicate key)
	       predicate))
	s e step check)
    (if from-end
	(setf
	 s end
	 e start
	 step #'1-
	 check #'<)
	(setf
	 s start
	 e end
	 step #'1+
	 check #'>))
    (cl:do* ((i s (funcall step i)))
	    ((funcall check i e))
      (let ((item (row-major-get array i)))
	(if (funcall p item)
	    (return-from find-if item))))))

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
    (let ((result (apply #'make (copy-arguments array))))
      (dotimes (i (total-size array))
	(setf (row-major-get result i) (row-major-get array i)))
      result)))

(defmacro do ((var array &optional result) &body body)
  `(loop for i from 0 below (total-size ,array)
	do (let ((,var (row-major-get ,array i)))
	     ,@body)
	finally (return ,result)))

;; Generic methods

(defmethod std.collection:get ((collection array) indexes)
  (apply #'get collection indexes))

(defmethod std.collection:put! ((collection array) indexes value)
  (let ((i (apply #'row-major-index collection indexes)))
    (row-major-put! collection i value)))

(defmethod std.base:copy ((object array))
  (copy object))

(defmethod std.collection:count (item (collection array) &key from-end (start 0) end key test test-not)
  (count item collection :from-end from-end :start start :end end :key key :test test :test-not test-not))

(defmethod std.collection:count-if (predicate (collection array) &key from-end (start 0) end key)
  (count-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:count-if-not (predicate (collection array) &key from-end (start 0) end key)
  (count-if-not predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:reduce (function (collection array) &key key from-end (start 0) end (initial-value nil initial-value-p))
  (apply #'reduce function collection :key key :from-end from-end :start start :end end (if initial-value-p (list :initial-value initial-value))))

(defmethod std.collection:find (item (collection array) &key from-end (start 0) end key test test-not)
  (find item collection :from-end from-end :start start :end end :key key :test test :test-not test-not))

(defmethod std.collection:find-if (predicate (collection array) &key from-end (start 0) end key)
  (find-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:find-if-not (predicate (collection array) &key from-end (start 0) end key)
  (find-if-not predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:sort ((collection array) predicate &key key)
  (sort (copy collection) predicate :key key))