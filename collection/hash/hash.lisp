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

(defmacro do (((key value) hash &optional result) &body body) ;TODO: Needs a with-gensyms on hash
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

(defun find-if (hash predicate &optional key)
  (let ((p (if key
	       (compose predicate key)
	       predicate)))
    (do ((nil value) hash)
	(when (funcall p value)
	    (return-from find-if value)))))

(defun find-if-not (hash predicate &optional key)
  (find-if hash (compose #'not predicate) key))

(defun find (hash item &key key test test-not)
  (cond
    (test (find-if hash (lambda (v) (funcall test v item)) key))
    (test-not (find-if-not hash (lambda (v) (funcall test-not v item)) key))
    (t (find-if hash (lambda (v) (eql v item)) key))))      

(defun-alias 'hash-table-count 'length)

(defun-alias 'maphash 'map)

;; Generic access

(defmethod std.collection:get ((container hash-table) key)
  (get container key))

(defmethod std.collection:put! ((container hash-table) key value)
  (put! container key value))

(defmethod std.base:copy ((container hash-table))
  (copy container))

(defmethod std.collection:find (item (container hash-table) &key from-end start end key test test-not)
  (declare (ignore from-end start end))
  (find container item :key key :test test :test-not test-not))

(defmethod std.collection:find-if (predicate (container hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (find-if container predicate key))

(defmethod std.collection:find-if-not (predicate (container hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (find-if-not container predicate key))

;; Read/write macros

(defun read-hash (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((buffer (array:make 0 :adjustable t :fill-pointer 0 :element-type 'character))
	result
	options
	data
	keyword
	in-quote
	in-double-quote)
    (labels ((emptyp ()
	       (= 0 (vector:length buffer)))
	     (save (char)
	       (vector-push-extend char buffer))
	     (current-string ()
	       (unless (emptyp)
		 (prog1
		     (read-from-string (copy-seq buffer))
		   (setf (array:fill-pointer buffer) 0))))
	     (handle-colon ()
	       (cond
		 (in-quote
		  (setf in-quote nil)
		  (save #\:))
		 (keyword (error "parse error, unexpected-colon"))
		 ((emptyp) (save #\:)) ; working with a keyword
		 (t (setf keyword (current-string)
			  in-quote t))))	; variable to a keyword is quoted
	     (handle-quote ()
	       (save #\')
	       (setf in-quote t))
	     (handle-double-quote ()
	       (setf in-double-quote (not in-double-quote))
	       (save #\"))
	     (handle-end-token ()
	       (let* ((token (current-string)))
		 (if in-quote (setf in-quote nil))
		 (when token
		   (cond
		     (keyword
		      (push keyword options)
		      (push token options)
		      (setf keyword nil))
		     (t (push token data))))))
	     (handle-char (char)
	       (case char
		 (#\: (handle-colon))
		 (#\' (handle-quote))
		 (#\" (handle-double-quote))
		 ((#\, #\Space) (handle-end-token))
		 (t (save char))))
	     (load-data (data)
	       (loop
		  for key in data by #'cddr
		  for val in (rest data) by #'cddr
		  do (put! result key val)))
	     (read-until (char)
	       (loop for ch = (read-char-no-hang stream)
		  when (and (eq ch char) (not in-double-quote))
		  do (progn
		       (handle-end-token)
		       (setf result (apply #'make (nreverse options)))
		       (load-data (nreverse data))
		       (return result))
		  do (if (and in-double-quote (not (eq ch #\")))
			 (save ch)
			 (handle-char ch)))))
      (read-until #\}))))

(defun write-hash (stream hash)
  (let ((pairs (loop
		  for key being the hash-keys of hash
		  for value being the hash-values of hash
		  collect (list key value))))
    (format stream "#{~{~{~S ~S~}~^, ~}}" pairs)))

(set-dispatch-macro-character #\# #\{ #'read-hash)
