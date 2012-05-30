(in-package #:std.collection.array)

(defstruct (subscripts
	     (:constructor %make-subscripts (current last-dimension max)))
  (current nil :type simple-vector)
  (last-dimension 0 :type fixnum :read-only t)
  (max nil :type simple-vector :read-only t))

(defun make-subscripts (array &optional start)
  (let* ((dimensions-list (array-dimensions array))
	 (dimensions-last (1- (cl:length dimensions-list)))
	 (dimensions-max (apply #'vector (mapcar #'1- dimensions-list)))
	 (start (if start
		    (if (eq start t) (copy dimensions-max) (apply #'vector start))
		    (new-from dimensions-max))))
    (%make-subscripts start dimensions-last dimensions-max)))

(macrolet ((def-advance (fun-name test-form advance-fun reset-form)
	     `(defun ,fun-name (subscripts)
	       (labels ((advance! (subscript-array i subscript-max)
			  (cond
			    ((< i 0) (return-from ,fun-name nil))
			    (,test-form (,advance-fun (svref subscript-array i)))
			    (t
			     (setf (svref subscript-array i) ,reset-form)
			     (advance! subscript-array (1- i) subscript-max)))))
		 (advance! (subscripts-current subscripts) (subscripts-last-dimension subscripts) (subscripts-max subscripts)))
	       subscripts)))
  (def-advance subscripts-inc! (< (svref subscript-array i) (svref subscript-max i)) incf 0)
  (def-advance subscripts-dec! (> (svref subscript-array i) 0) decf (svref subscript-max i)))

(defun valid-subscript-index (array subscript-list)
  (let ((max-end (copy-seq (array-dimensions array))))
    (cl:do ((c max-end (cdr c)))
	   ((null (cdr c)))
      (decf (car c)))
    (if (equal subscript-list max-end)
	(total-size array)
	(apply #'row-major-index array subscript-list))))