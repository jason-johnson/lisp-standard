(in-package #:std.collection.bit-vector)

;; Normal access

(defun get (bit-vector index)
  (bit bit-vector index))

(defsetf get (bit-vector index) (value)
  `(setf (bit ,bit-vector ,index) ,value))

(defun put! (bit-vector index value)
  (setf (get bit-vector index) value))

;; Generic access

