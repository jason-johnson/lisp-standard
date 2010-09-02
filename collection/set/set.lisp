(in-package #:std.collection.set)

;;  Normal access

(defconstant +dummy-set-value+ '.dummy-set-value.)

(defun setp (set)
  (and
   (hashp set)
   (loop for val being the hash-values of set always (eq val +dummy-set-value+))))

(deftype set () '(satisfies setp))	; independant of representation

(defun member (set member)
  (eq (hash:get set member) +dummy-set-value+))

(defun add (set member)
  (hash:put! set member +dummy-set-value+)
  member)

(defmacro do ((var set &optional result) &body body)
  `(loop for ,var being the hash-keys of ,set
	do (progn
	     ,@body)
	finally (return ,result)))

(defun-alias 'hash:copy 'copy)

;; Set operations (note: most of these become independent of representation after conversion to the macro)

(defun union! (set1 set2)
  (do (member set2 set1)
    (add-member set1 member)))

(defun union (set1 set2)
  (union! (copy-set set1) set2))

(defun intersection! (set1 set2)
  (do (member set2)
    (if (member set1 member)
	(setf (href set1 member) 1)))
  (do (member set1 set1)
    (if (eq (href set1 member) 1)
	(setf (href set1 member) +dummy-set-value+)
	(hdelete! set1 member))))

(defun intersection (set1 set2)
  (intersection! (copy-set set1) set2))

(defun complement! (set1 set2)
  (do (member set2 set1)
    (hash:remove! set1 member)))

(defun complement (set1 set2)
  (complement! (copy-set set1) set2))

(defun cartesian-product (set1 set2)
  (let ((result (copy-hash-options set1)))
    (do (member1 set1 result)
      (do (member2 set2)
	(add result (cons member1 member2))))))

(defun subsetp (set1 set2)
  (do (member set1 t)
    (unless (member set2 member)
      (return nil))))

;; Read/write macros

(defun read-set (stream subchar arg)
  (declare (ignore subchar)
	   (ignore arg))
  (let ((keywords (list (cons :test 'eql)))
	atoms)
    (labels ((assoc->list (assoc)
	       (list (first assoc) (rest assoc)))
	     (set-keyword (key value)
	       (ccase key
		 ((:test :size :rehash-size :rehash-threshold)
		  (if (assoc key keywords)
		      (rplacd (assoc key keywords) value)
		      (push (cons key value) keywords)))))
	     (parse (token stream)
	       (ctypecase token
		 (keyword (set-keyword token (read stream t)))
		 (t (push token atoms)))))
      (loop
	 for token = (read stream nil)
	 while token
	 do (parse token stream))
      (let ((set (apply #'make-hash-table (apply #'append (mapcar #'assoc->list keywords)))))
	(loop
	   for member in atoms
	   do (add-member set member))
	set))))

(defun write-set (stream set)
  (let ((format-string "#[~:[~;(:test ~a) ~]~{~S~^ ~}]") 
	(test (hash-table-test set))
	(atoms (loop
		  for key being the hash-keys of set
		  collect key)))
    (if (eq test 'eql)
	(format stream format-string nil atoms)
	(format stream format-string t test atoms))))

(set-dispatch-macro-character #\# #\[ #'read-set)

(set-macro-character #\] (lambda (stream char)
			   (declare (ignore stream)
				    (ignore char))
				    nil))
