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

(declaim (inline get length find))

(defun get (array &rest subscripts)
  (apply #'aref array subscripts))

(defsetf get (array &rest subscripts) (value)
  `(setf (aref ,array ,@subscripts) ,value))

; NOTE: We don't define a put! since we can't sensibly define one with &rest subscripts

(defun-alias 'total-size 'length)

;; (defun length (array)			; TODO: Is this what we want?  We already have total-size for total size, but this is different than everything else we do with arrays
;;   (first (dimensions array)))		; TODO: I don't think so.  Since all functions use the total-size, length wouldn't be useful for anything

;; These functions should be tested to see if coerce is too slow vs modifying a list via pointers directly
(defun map (function &rest arrays)
  (let* ((dimensions-list (apply #'mapcar #'min (mapcar #'dimensions arrays)))
	 (result (make dimensions-list))
	 (s (make-subscripts result)))
    (cl:do ((subscripts s (subscripts-inc! subscripts)))
	   ((null subscripts) result)
      (let* ((ss (coerce (subscripts-current subscripts) 'list))
	     (items (mapcar (lambda (array) (apply #'get array ss)) arrays))
	     (e (apply function items)))
	(setf (apply #'aref result ss) e)))))

(defun position-if (predicate array &key from-end start end key)
  (when end (ensure-valid-end-subscript array end)) ; NOTE: Would be nice if the compiler could do this when possible
  (let* ((has-end? (if from-end (not (null end)) (not (null start))))
	 (start (make-subscripts array start))
	 (end (make-subscripts array (or end t)))
	 (last-dimension (subscripts-last-dimension start))
	 (g (if key
		(compose key #'get)
		#'get))
	 (end-index 0)
	 s e advance)
    (if from-end
	(setf s (subscripts-dec! end)
	      e (subscripts-dec! start)
	      has-end? (when e has-end?)
	      advance #'subscripts-dec!)
	(setf s start
	      e end
	      advance #'subscripts-inc!))
    (labels ((ssref (subscript index)
	       (svref (subscripts-current subscript) index))
	     (%finished? (subscripts)	; Here we take advantage of the fact that once a left-most position reaches the expected number it won't increase anymore and won't need to be checked further
	       (when (eql (ssref e end-index) (ssref subscripts end-index))
		 (if (eql end-index last-dimension)
		     t
		     (progn
		       (incf end-index)
		       (%finished? subscripts)))))
	     (finished? (subscripts)
	       (when has-end? (%finished? subscripts))))
      (cl:do ((subscripts s (funcall advance subscripts)))
	     ((or (null subscripts) (finished? subscripts)) nil)
	(let* ((ss (coerce (subscripts-current subscripts) 'list))
	       (item (apply g array ss)))
	  (when (funcall predicate item)
	    (return-from position-if ss)))))))

(defun position (item array &key from-end start end key (test #'eql))
  (position-if (lambda (e) (funcall test item e)) array :from-end from-end :start start :end end :key key))

(defmacro traverse-array-as-vector ((array item start end key from-end? result &key get-item-in-varlist from-end-forms from-start-forms pre-do-forms do-var-forms) &body body)
  "Traverse ARRAY linear as a vector"
  (let (less-than greater-than)
    (if get-item-in-varlist
	(setf
	 less-than '#'<=
	 greater-than '#'>=)
	(setf
	 less-than '#'<
	 greater-than '#'>))
    (with-unique-names (g s e step check i)
      `(macrolet ((%get (i)
		    `(funcall ,',g ,',array ,i))
		  (%step (i)
		    `(funcall ,',step ,i)))
	 (symbol-macrolet (($start ,s))
	   (setf ,end (if ,end (1- ,end) (1- (total-size ,array))))
	   (let ((,g (if ,key
			 (compose ,key #'row-major-get)
			 #'row-major-get))
		 ,s ,e ,step ,check)
	     (if ,from-end?
		 (setf
		  ,@from-end-forms
		  ,s ,end
		  ,e ,start
		  ,step #'1-
		  ,check ,less-than)
		 (setf
		  ,@from-start-forms
		  ,s ,start
		  ,e ,end
		  ,step #'1+
		  ,check ,greater-than))
	     ,@pre-do-forms
	     (do* ((,i ,s (%step ,i))
		   ,@(when get-item-in-varlist `((,item (%get ,i) (%get ,i))))
		   ,@do-var-forms)
		  ((funcall ,check ,i ,e) ,result)
	       ,@(if get-item-in-varlist
		     body
		     `((let ((,item (%get ,i)))
			 ,@body))))))))))

(defmacro traverse-array-as-vector-with-predicate ((array predicate item start end key from-end? result) &body body)
  `(traverse-array-as-vector (,array ,item ,start ,end ,key ,from-end? ,result)
    (when (funcall ,predicate ,item)
      ,@body)))

(defun count-if (predicate array &key from-end (start 0) end key)
  (let ((count 0))
    (traverse-array-as-vector-with-predicate (array predicate item start end key from-end count)
      (incf count))))

(defun count (item array &key from-end (start 0) end key (test #'eql))
  (count-if (lambda (v) (funcall test v item)) array :from-end from-end :start start :end end :key key))

(defun reduce (function array &key key from-end (start 0) end (initial-value nil initial-value-p))
  (let ((last-result initial-value)
	f)
    (traverse-array-as-vector (array item start end key from-end result
				     :get-item-in-varlist t
				     :from-end-forms (f (lambda (r v) (funcall function v r)))
				     :from-start-forms (f function)
				     :pre-do-forms ((unless initial-value-p
						      (setf last-result (%get $start)
							    $start (%step $start))))
				     :do-var-forms ((result (funcall f last-result item) (funcall f result item)))))))

(defun find-if (predicate array &key from-end (start 0) end key)
  (traverse-array-as-vector-with-predicate (array predicate item start end key from-end nil)
    (return-from find-if item)))

(defun find (item array &key from-end (start 0) end key (test #'eql))
  (find-if (lambda (v) (funcall test v item)) array :from-end from-end :start start :end end :key key))

(defun new-from (array &key (dimensions (dimensions array)) (element-type (element-type array)) (adjustable (adjustable-p array) adjustable?) (fill-pointer nil fill-pointer?) displaced-to displaced-index-offset)
  (let ((options (list element-type :element-type dimensions)))
    (macrolet ((when-put! (test key value)
		 `(when ,test
		    (push ,key options)
		    (push ,value options))))
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