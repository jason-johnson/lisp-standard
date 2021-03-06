(in-package #:std.collection.set)

;; Setup for set type

(defstruct (set
	     (:predicate setp))		;TODO: Much of this doesn't support this new method, we need a make function, etc.
  data)

;; Private

(defconstant +dummy-set-value+ '.dummy-set-value.)

(declaim (inline get put!))

(defun get (set member)
  (hash:get (set-data set) member))

(defun put! (set member)
  (hash:put! (set-data set) member +dummy-set-value+))

; NOTE: It seems I have to do this because I use a struct for implementation and if you define a struct as a constant or literal CL forces you to define how to load it
(defmethod make-load-form ((object set) &optional environment)
  (make-load-form-saving-slots object :environment environment))

;; Set specific

(declaim (inline clear size test rehash-size rehash-threshold options))

(defun clear (set)
  (hash:clear (set-data set))
  set)
    
(defun size (set)
  (hash:size (set-data set)))

(defun test (set)
  (hash:test (set-data set)))

(defun rehash-size (set)
  (hash:rehash-size (set-data set)))

(defun rehash-threshold (set)
  (hash:rehash-threshold (set-data set)))

(defun options (set)
  (hash:options (set-data set)))

;;  Normal access

(declaim (inline memberp add remove! remove))

(defun make (&key (test 'eql) (size 16) (rehash-size 1.5) (rehash-threshold 1) hash-function synchronized)
  (make-set :data (hash:make
		   :test test
		   :size size
		   :rehash-size rehash-size
		   :rehash-threshold rehash-threshold
		   :hash-function hash-function
		   :synchronized synchronized)))

(defun memberp (set member)
  (eq (get set member) +dummy-set-value+))

(defun add (set member)
  (put! set member)
  member)

(defmacro do ((var set &optional result) &body body)
  `(loop for ,var being the hash-keys of (set-data ,set)
	do (progn
	     ,@body)
	finally (return ,result)))

(defun copy (set)
  (let ((result (apply #'make (options set))))
    (do (member set result)
	(add result member))))

(defun remove! (set member)
  (hash:remove! (set-data set) member))

(defun remove (set member)
  (let ((result (copy set)))
    (remove! result member)
    result))

(defun length (set)
  (hash:length (set-data set)))

(impl-common.unordered:define-collection-functions set do reduce count count-if count-if-not find find-if find-if-not)

(defun map (fun set)			; NOTE: Maping over more than one unordered set makes no sense
  (let ((result (apply #'make (options set))))
    (do (e set result)
	(add result (funcall fun e)))))

;; Set operations

(defun union! (set1 set2)
  (do (member set2 set1)
    (add set1 member)))

(defun union (set1 set2)
  (union! (copy set1) set2))

(defun intersection! (set1 set2)
  (do (member set2)
    (if (memberp set1 member)
	(hash:put! (set-data set1) member 1)))
  (do (member set1 set1)
    (if (eq (get set1 member) 1)
	(put! set1 member)
	(remove! set1 member))))

(defun intersection (set1 set2)
  (intersection! (copy set1) set2))

(defun complement! (set1 set2)
  (do (member set2 set1)
    (remove! set1 member)))

(defun complement (set1 set2)
  (complement! (copy set1) set2))

(defun cartesian-product (set1 set2)
  (let ((result (apply #'make (options set1))))
    (do (member1 set1 result)
      (do (member2 set2)
	(add result (cons member1 member2))))))

(defun subsetp (set1 set2)
  (do (member set1 t)
    (unless (memberp set2 member)
      (return nil))))

;; Generic access

(defmethod std.base:copy ((collection set))
  (copy collection))

(defmethod std.collection:count (item (collection set) &key from-end start end key test test-not)
  (declare (ignore from-end start end))
  (count item collection :key key :test test :test-not test-not))

(defmethod std.collection:count-if (predicate (collection set) &key from-end start end key)
  (declare (ignore from-end start end))
  (count-if predicate collection key))

(defmethod std.collection:count-if-not (predicate (collection set) &key from-end start end key)
  (declare (ignore from-end start end))
  (count-if-not predicate collection key))

(defmethod std.collection:reduce (function (collection set) &key key from-end start end (initial-value nil initial-value-p))
  (declare (ignore start end))
  (apply #'reduce function collection :key key :from-end from-end (if initial-value-p (list :initial-value initial-value))))

(defmethod std.collection:find (item (collection set) &key from-end start end key test test-not)
  (declare (ignore from-end start end))
  (find item collection :key key :test test :test-not test-not))

(defmethod std.collection:find-if (predicate (collection set) &key from-end start end key)
  (declare (ignore from-end start end))
  (find-if predicate collection key))

(defmethod std.collection:find-if-not (predicate (collection set) &key from-end start end key)
  (declare (ignore from-end start end))
  (find-if-not predicate collection key))

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
      (let ((set (apply #'make (apply #'append (mapcar #'assoc->list keywords)))))
	(loop
	   for member in atoms
	   do (add set member))
	set))))

(defun write-set (stream set)
  (let ((atoms (loop
		  for key being the hash-keys of (set-data set)
		  collect key)))
    (format stream "#[~{~S~^ ~}]" atoms)))

(set-dispatch-macro-character #\# #\[ #'read-set)

(set-macro-character #\] (lambda (stream char)
			   (declare (ignore stream)
				    (ignore char))
				    nil))
