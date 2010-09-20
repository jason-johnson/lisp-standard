(in-package #:std.collection.vector)

;; Vector specific

(defun-alias 'vector-push 'push!)
(defun-alias 'vector-push-extend 'push-extend!)
(defun-alias 'vector-pop 'pop!)

;; Normal access

(declaim (inline get put! sort stable-sort merge^ merge concatenate))

(defun get (vector index)
  (aref vector index))

(defsetf get (vector index) (value)
  `(setf (aref ,vector ,index) ,value))

(defun put! (vector index value)
  (setf (get vector index) value))

(defmacro do ((var array &optional result) &body body)
  `(loop for ,var across ,array
	do (progn
	     ,@body)
	finally (return ,result)))

(defun map (fun &rest vectors)
  (apply #'map-to 'vector fun (first vectors) (rest vectors)))

(defun sort (vector predicate &key key)
  (cl:sort (copy vector) predicate :key key))

(defun stable-sort (vector predicate &key key)
  (cl:stable-sort (copy vector) predicate :key key))

(defun merge^ (vector1 vector2 predicate &key key)
  (cl:merge 'vector vector1 vector2 predicate :key key))

(defun merge (vector1 vector2 predicate &key key)
  (merge^ (copy vector1) (copy vector2) predicate :key key))

(defun concatenate (&rest sequences)
  (apply #'cl:concatenate 'vector sequences))

;; Generic access

(defmethod std.collection:get ((collection vector) index)
  (get collection index))

(defmethod std.collection:put! ((collection vector) index value)
  (setf (get collection index) value))

;; NOTE: vector and simple-vector are subtypes of array so the copy defined in the array module works for us too.  This also requires us to override various generic methods

(defmethod std.collection:count (item (collection vector) &key from-end (start 0) end key (test #'eql) (test-not nil test-not-p))
  (apply #'count item collection :from-end from-end :start start :end end :key key
	 (if test-not-p
	     (list :test-not test-not)
	     (list :test test))))

(defmethod std.collection:count-if (predicate (collection vector) &key from-end (start 0) end key)
  (count-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:count-if-not (predicate (collection vector) &key from-end (start 0) end key)
  (count-if-not predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:reduce (function (collection vector) &key key from-end (start 0) end (initial-value nil initial-value-p))
  (apply #'reduce function collection :key key :from-end from-end :start start :end end (if initial-value-p (list :initial-value initial-value))))

(defmethod std.collection:find (item (collection vector) &key from-end (start 0) end key test test-not)
  (find item collection :from-end from-end :test test :test-not test-not :start start :end end :key key))

(defmethod std.collection:find-if (predicate (collection vector) &key from-end (start 0) end key)
  (find-if predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:find-if-not (predicate (collection vector) &key from-end (start 0) end key)
  (find-if-not predicate collection :from-end from-end :start start :end end :key key))

(defmethod std.collection:sort ((collection vector) predicate &key key)
  (sort collection predicate :key key))

(defmethod std.collection:stable-sort ((collection vector) predicate &key key)
  (stable-sort collection predicate :key key))

(defmethod std.collection:merge ((output-spec (eql 'vector)) (collection1 vector) (collection2 vector) predicate &key key)
  (merge collection1 collection2  predicate :key key))