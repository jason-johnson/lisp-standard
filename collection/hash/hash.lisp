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

(defun reduce (function hash &key key from-end (initial-value nil initial-value-p))
  (let ((last-result initial-value)
	(has-value initial-value-p)
	(f (if from-end
	       (lambda (r v) (funcall function v r))
	       function)))
    (do ((nil value) hash last-result)
	(let ((v (if key		; TODO: Would it be faster to set key to id by default and always call it?
		     (funcall key value)
		     value)))
	      (setf last-result
		    (if has-value
			(funcall f last-result v)
			v)
		    has-value t)))))

(defun count-if (predicate hash &optional key)
  (let ((p (if key
	       (compose predicate key)
	       predicate))
	(count 0))
    (do ((nil value) hash count)
	(when (funcall p value)
	  (incf count)))))

(defun count-if-not (predicate hash &optional key)
  (count-if (compose #'not predicate) hash key))

(defun count (item hash &key key test test-not)
  (cond
    (test (count-if (lambda (v) (funcall test v item)) hash key))
    (test-not (count-if-not (lambda (v) (funcall test-not v item)) hash key))
    (t (count-if (lambda (v) (eql v item)) hash key))))

(defun find-if (predicate hash &optional key)
  (let ((p (if key
	       (compose predicate key)
	       predicate)))
    (do ((nil value) hash)
	(when (funcall p value)
	    (return-from find-if value)))))

(defun find-if-not (predicate hash &optional key)
  (find-if (compose #'not predicate) hash key))

(defun find (item hash &key key test test-not)
  (cond
    (test (find-if (lambda (v) (funcall test v item)) hash key))
    (test-not (find-if-not (lambda (v) (funcall test-not v item)) hash key))
    (t (find-if (lambda (v) (eql v item)) hash key))))

(defun-alias 'hash-table-count 'length)

(defun-alias 'maphash 'map)

;; Generic access

(defmethod std.collection:get ((collection hash-table) key)
  (get collection key))

(defmethod std.collection:put! ((collection hash-table) key value)
  (put! collection key value))

(defmethod std.base:copy ((collection hash-table))
  (copy collection))

(defmethod std.collection:reduce (function (collection hash-table) &key key from-end start end (initial-value nil initial-value-p))
  (declare (ignore start end))
  (apply #'reduce function collection :key key :from-end from-end (if initial-value-p (list :initial-value initial-value))))

(defmethod std.collection:find (item (collection hash-table) &key from-end start end key test test-not)
  (declare (ignore from-end start end))
  (find item collection :key key :test test :test-not test-not))

(defmethod std.collection:find-if (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (find-if predicate collection key))

(defmethod std.collection:find-if-not (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (find-if-not predicate collection key))

(defmethod std.collection:count (item (collection hash-table) &key from-end start end key test test-not)
  (declare (ignore from-end start end))
  (count item collection :key key :test test :test-not test-not))

(defmethod std.collection:count-if (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (count-if predicate collection key))

(defmethod std.collection:count-if-not (predicate (collection hash-table) &key from-end start end key)
  (declare (ignore from-end start end))
  (count-if-not predicate collection key))

;; NOTE: count ignored because it makes no sense.  If the user wants to remove all but one they can use remove-duplicates
;; (defmethod std.collection:remove^ (item (collection hash-table) &key from-end test test-not start end count key)
;;   (declare (ignore from-end start end count))
;;   (cond
;;     (test (remove-if^ (lambda (v) (funcall test v item)) collection key))
;;     (test-not (remove-if-not^ (lambda (v) (funcall test-not v item)) collection key))
;;     (t (remove-if^ (lambda (v) (eql v item)) collection key))))

;; (defmethod std.collection:remove (item (collection hash-table) &key from-end test test-not start end count key)
;;   (declare (ignore from-end start end count))
;;   (cond
;;     (test (remove-if (lambda (v) (funcall test v item)) collection key))
;;     (test-not (remove-if-not (lambda (v) (funcall test-not v item)) collection key))
;;     (t (remove-if (lambda (v) (eql v item)) collection key))))

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
