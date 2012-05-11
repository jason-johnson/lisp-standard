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
  `(loop
      for ,var across ,array
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

(defun concatenate (&rest vectors)
  (apply #'cl:concatenate 'vector vectors))


;; Generic access

(defmethod std.collection:get ((collection vector) index)
  (get collection index))

(defmethod std.collection:put! ((collection vector) index value)
  (setf (get collection index) value))

;; NOTE: vector and simple-vector are subtypes of array so the copy defined in the array module works for us too.

(defmethod std.collection:sort ((collection vector) predicate &key key)
  (sort collection predicate :key key))

(defmethod std.collection:stable-sort ((collection vector) predicate &key key)
  (stable-sort collection predicate :key key))

(defmethod std.collection:merge ((output-spec (eql 'vector)) (collection1 vector) (collection2 vector) predicate &key key)
  (merge collection1 collection2  predicate :key key))