(in-package #:std.collection.vector)

;; Vector specific

(defun-alias 'vector-push 'push)
(defun-alias 'vector-push-extend 'push-extend)
(defun-alias 'vector-pop 'pop)

;; Normal access

(declaim (inline get put!))

(defun get (vector index)
  (aref vector index))

(defsetf get (vector index) (value)
  `(setf (aref ,vector ,index) ,value))

(defun put! (vector index value)
  (setf (get vector index) value))

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

;; Generic access

(defmethod std.collection:get ((container vector) index)
  (get container index))

(defmethod std.collection:put! ((container vector) index value)
  (setf (get container index) value))

;; NOTE: vector and simple-vector are subtypes of array so the copy defined in the array module works for us too

(defmethod std.collection:sort ((container vector) predicate &key key)
  (sort container predicate :key key))

(defmethod std.collection:stable-sort ((container vector) predicate &key key)
  (stable-sort container predicate :key key))

(defmethod std.collection:merge ((container1 vector) (container2 vector) predicate &key key)
  (merge container1 container2  predicate :key key))
