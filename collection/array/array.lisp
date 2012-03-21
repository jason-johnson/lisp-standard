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

(defun-alias 'total-size 'length)

;; (defun length (array)			; TODO: Is this what we want?  We already have total-size for total size, but this is different than everything else we do with arrays
;;   (first (dimensions array)))		; TODO: I don't think so.  Since all functions use the total-size, length wouldn't be useful for anything

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

(defmacro traverse-array-as-vector ((array get length item start end key from-end? result &key from-end-forms from-start-forms pre-do-forms do-var-forms) &body body)
  "Traverse ARRAY linear as a vector"
  (with-gensyms (g s e step check i)
    `(macrolet ((%get (i)
		  `(funcall ,',g array ,i))
		(%step (i)
		  `(funcall ,',step ,i)))
       (symbol-macrolet (($start ,s))
	 (progn
	   (unless ,end (setf ,end (1- (funcall ,length ,array))))
	   (let ((,g (if ,key
			 (compose ,key ,get)
			 ,get))
		 ,s ,e ,step ,check)
	     (if ,from-end?
		 (setf
		  ,@from-end-forms
		  ,s ,end
		  ,e ,start
		  ,step #'1-
		  ,check #'<=)
		 (setf
		  ,@from-start-forms
		  ,s ,start
		  ,e ,end
		  ,step #'1+
		  ,check #'>=))
	     ,@pre-do-forms
	     (do* ((,i ,s (%step ,i))
		   (,item (%get ,i) (%get ,i))
		   ,@do-var-forms)
		  ((funcall ,check ,i ,e) ,result)
	       ,@body)))))))

(defun reduce (function array &key key from-end (start 0) end (initial-value nil initial-value-p))
  (let ((last-result initial-value)
	f)
    (traverse-array-as-vector (array #'row-major-get #'total-size item start end key from-end result
			    :from-end-forms (f (lambda (r v) (funcall function v r)))
			    :from-start-forms (f function)
			    :pre-do-forms ((unless initial-value-p
					    (setf last-result (%get $start)
						  $start (%step $start))))
			    :do-var-forms ((result (funcall f last-result item) (funcall f result item)))))))

(defun find-if (predicate array &key from-end (start 0) end key)
  (traverse-array (array start end total-size predicate key from-end #'row-major-get item nil)
    (return-from find-if item)))

(defun find-if-not (predicate array &key from-end (start 0) end key)
  (find-if (complement predicate) array :from-end from-end :start start :end end :key key))

(defun find (item array &key from-end (start 0) end key test test-not)
  (cond
    (test (find-if (lambda (v) (funcall test v item)) array :from-end from-end :start start :end end :key key))
    (test-not (find-if-not (lambda (v) (funcall test-not v item)) array :from-end from-end :start start :end end :key key))
    (t (find-if (lambda (v) (eql v item)) array :from-end from-end :start start :end end :key key))))

(defun new-from (array &key (dimensions (dimensions array)) (element-type (element-type array) element-type?) (adjustable (adjustable-p array) adjustable?) (fill-pointer nil fill-pointer?) displaced-to displaced-index-offset)
  (let ((options (list dimensions)))
    (macrolet ((when-put! (test key value)
		 `(when ,test
		    (push ,key options)
		    (push ,value options))))
      (when-put! element-type? :element-type element-type)
      (when-put! adjustable? :adjustable adjustable)
      (when-put! fill-pointer? :fill-pointer fill-pointer)
      (when-put! displaced-to :displaced-to displaced-to)
      (when-put! displaced-index-offset :displaced-index-offset displaced-index-offset))
    (apply #'make (nreverse options))))

(defun copy (array)
  (let ((result (multiple-value-bind (da di) (displacement array)
		  (if da
		      (new-from array :displaced-to da :displaced-index-offset di)
		      (new-from array)))))
    (dotimes (i (total-size array))
      (setf (row-major-get result i) (row-major-get array i)))
    result))

(defmacro do ((var array &optional result) &body body)
  `(loop for i from 0 below (total-size ,array)
	do (let ((,var (row-major-get ,array i)))
	     ,@body)
	finally (return ,result)))