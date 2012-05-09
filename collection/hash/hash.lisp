(in-package #:std.collection.hash)

;; Hash specific

(defun options (hash)
  (list
   :test (hash-table-test hash)
   :size (hash-table-size hash)
   :rehash-size (hash-table-rehash-size hash)
   :rehash-threshold (hash-table-rehash-threshold hash)))

(defun-alias 'clrhash 'clear)
(defun-alias 'hash-table-p 'hashp)
(defun-alias 'hash-table-size 'size)
(defun-alias 'hash-table-test 'test)
(defun-alias 'hash-table-rehash-size 'rehash-size)
(defun-alias 'hash-table-rehash-threshold 'rehash-threshold)
(defun-alias 'sxhash 'hash)

;;  Normal access

(defun-alias 'make-hash-table 'make)

(declaim (inline get put! remove!))

(defun get (hash key &optional default)
  (gethash key hash default))

(defsetf get (hash key) (value)
  `(setf (gethash ,key ,hash) ,value))

(defun put! (hash key value)
  (setf (get hash key) value))

(defun remove (hash key)
  (let* ((result (copy hash))
	 (value (get hash key))
	 (deleted (remove! result key)))
    (values result deleted value)))

(defun remove! (hash key)
  (remhash key hash))

;; Actions for the whole hash

(defun-alias 'hash-table-count 'length)

(defun-alias 'maphash 'map)

(defmacro do (((key value) hash &optional result) &body body)
  "Iterate over a hash.  key or value can be set to nil if not used"
  `(loop
      ,@(if key `(for ,key being the hash-keys of ,hash))
      ,@(if value `(for ,value being the hash-values of ,hash))
	do (progn
	     ,@body)
	finally (return ,result)))

(defun copy (hash)
  (let ((result (apply #'make-hash-table (options hash))))
    (do ((key value) hash result)
	(put! result key value))))

;; Operations across pairs

;; TODO TODO TODO: The following is wrong as it conses key and value together before passing to callers.  This would probably be required with reduce but for others the predicate/whatever should take two values
;; TODO TODO TODO: But check this logic before changing too much
(impl-common.unordered:define-collection-functions hash do
  :value (k v)
  :value-form (cons k v)
  :reduce reduce
  :count count
  :count-if count-if
  :count-if-not count-if-not
  :find find
  :find-if find-if
  :find-if-not find-if-not)

;; Read/write macros

(defun read-hash (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((crash-on-rb nil)
	 (*end-action* (lambda () (when crash-on-rb (sb-int:simple-reader-error stream "unexpected termination character"))))
	 (end (gensym "END"))
	 (comma (gensym "COMMA"))
	 (*readtable* (copy-readtable)))
    (declare (special *end-action*))
    (set-macro-character #\} (lambda (stream subchar)
			       (declare (ignore stream subchar))
			       (funcall *end-action*)))
    (flet ((read-first-key ()
	     (prog1
		 (read stream nil nil t)
	       (setf crash-on-rb t)))
	   (read-seperator ()
	     (let ((*readtable* (copy-readtable))
		   (*end-action* (lambda () end)))
	       (declare (special *end-action*))
	       (set-macro-character #\, (lambda (stream char)
					  (declare (ignore char) (ignore stream))
					  comma))
	       (read stream nil))))
      (let* ((first-key (read-first-key))
	     (equal 'eql)
	     (data (when first-key
		     (loop
			for key = first-key then (read stream nil nil t)
			for val = (read stream nil nil t)
			for end? = (read-seperator)
			unless (or (eq end? comma) (eq end? end)) do (sb-int:simple-reader-error stream ", or } expected")
			when (typep key 'string) do (setf equal 'equal)
			collect (list key val)
			until (eq end? end))))
	     (result (make :test equal)))
	(loop
	   for (key val) in data
	   do (put! result key val))
	result))))

(defun write-hash (stream hash)
  (let ((pairs (loop
		  for key being the hash-keys of hash
		  for value being the hash-values of hash
		  collect (list key value))))
    (format stream "#{~{~{~S ~S~}~^, ~}}" pairs)))

(set-dispatch-macro-character #\# #\{ #'read-hash)
