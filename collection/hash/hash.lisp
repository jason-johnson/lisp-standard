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
  (let ((buffer (vector:make 0 :adjustable t :fill-pointer 0 :element-type 'character))
	(equal 'eql)
	(key? t)
	result
	data
	in-double-quote)
    (labels ((empty? ()
	       (= 0 (vector:length buffer)))
	     (save (char)
	       (vector-push-extend char buffer))
	     (current-string ()
	       (unless (empty?)
		 (prog1
		     (read-from-string (copy-seq buffer))
		   (setf (vector:fill-pointer buffer) 0))))
	     (handle-double-quote ()
	       (when key?
		 (setf equal 'equal))
	       (setf in-double-quote (not in-double-quote))
	       (save #\"))
	     (handle-end-token ()
	       (setf key? (not key?))
	       (let ((token (current-string)))
		 (when token
		   (push token data))))
	     (handle-char (char)
	       (case char
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
		       (setf result (make :test equal))
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
