(in-package #:std.collection.hash)

; TODO: Should this actually be called "dictionary" and behave like set does (i.e. let the user pick which dictionary implementation)?  The only issue with that is; people will probably want to use different implementations in the same
; TODO: program, e.g. for one variable a hash, for another a red-black tree

;;  Normal access

(defun-alias 'make-hash-table 'make)

(declaim (inline get put! remove!))

(defun get (hash key)
  (gethash key hash))

(defsetf get (hash key) (value)
  `(setf (gethash ,key ,hash) ,value))

(defun put! (hash key value)
  (setf (get hash key) value))

(defun copy (hash)
  (let ((result 
	 (make-hash-table
	  :test (hash-table-test hash)
	  :size (hash-table-size hash)
	  :rehash-size (hash-table-rehash-size hash)
	  :rehash-threshold (hash-table-rehash-threshold hash))))
    (loop for key being the hash-keys of hash
       for value being the hash-values of hash
       do (put! result key value))
    result))

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

;; Generic access

(defmethod std.collection:get ((container hash-table) key)
  (get container key))

(defmethod std.collection:put! ((container hash-table) key value)
  (put! container key value))

(defmethod std.base:copy ((container hash-table))
  (copy container))

;; Read/write macros

(defun read-hash (stream subchar arg)
  (declare (ignore subchar)
	   (ignore arg))
  (let ((keywords (list (cons :test 'eql)))
	pairs)
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
		 (cons (push token pairs))
		 (keyword (set-keyword token (read stream t))))))
      (loop
	 for token = (read stream nil)
	 while token
	 do (parse token stream))
      (let ((hash (apply #'make-hash-table (apply #'append (mapcar #'assoc->list keywords)))))
	(loop
	   for (key . value) in pairs
	   do (put! hash key value))
	hash))))

(defun write-hash (stream hash)
  (let ((format-string "#{~:[~;:test ~a ~]~{~{(~S . ~S)~}~^ ~}}") 
	(test (hash-table-test hash))
	(pairs (loop
		  for key being the hash-keys of hash
		  for value being the hash-values of hash
		  collect (list key value))))
    (if (eq test 'eql)
	(format stream format-string nil pairs)
	(format stream format-string t test pairs))))

(set-dispatch-macro-character #\# #\{ #'read-hash)

(set-macro-character #\} (lambda (stream char)
			   (declare (ignore stream)
				    (ignore char))
				    nil))