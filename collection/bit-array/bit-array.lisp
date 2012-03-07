(in-package #:std.collection.bit-vector)

;; Bit-vector specific

(declaim (inline and nand or nor xor xnor not andc1 andc2 orc1 orc2))

(defun and (bit-vector-1 bit-vector-2)
  (bit-and bit-vector-1 bit-vector-2))

(defun nand (bit-vector-1 bit-vector-2)
  (bit-nand bit-vector-1 bit-vector-2))

(defun or (bit-vector-1 bit-vector-2)
  (bit-ior bit-vector-1 bit-vector-2))

(defun nor (bit-vector-1 bit-vector-2)
  (bit-nor bit-vector-1 bit-vector-2))

(defun xor (bit-vector-1 bit-vector-2)
  (bit-xor bit-vector-1 bit-vector-2))

(defun xnor (bit-vector-1 bit-vector-2)
  (bit-eqv bit-vector-1 bit-vector-2))

(defun not (bit-vector)
  (bit-not bit-vector))

(defun andc1 (bit-vector-1 bit-vector-2)
  (bit-andc1 bit-vector-1 bit-vector-2))

(defun andc2 (bit-vector-1 bit-vector-2)
  (bit-andc2 bit-vector-1 bit-vector-2))

(defun orc1 (bit-vector-1 bit-vector-2)
  (bit-orc1 bit-vector-1 bit-vector-2))

(defun orc2 (bit-vector-1 bit-vector-2)
  (bit-orc2 bit-vector-1 bit-vector-2))

(declaim (inline and! nand! or! nor! xor! xnor! not! andc1! andc2! orc1! orc2!))

(defun and! (bit-vector-1 bit-vector-2)
  (bit-and bit-vector-1 bit-vector-2 t))

(defun nand! (bit-vector-1 bit-vector-2)
  (bit-nand bit-vector-1 bit-vector-2 t))

(defun or! (bit-vector-1 bit-vector-2)
  (bit-ior bit-vector-1 bit-vector-2 t))

(defun nor! (bit-vector-1 bit-vector-2)
  (bit-nor bit-vector-1 bit-vector-2 t))

(defun xor! (bit-vector-1 bit-vector-2)
  (bit-xor bit-vector-1 bit-vector-2 t))

(defun xnor! (bit-vector-1 bit-vector-2)
  (bit-eqv bit-vector-1 bit-vector-2 t))

(defun not! (bit-vector)
  (bit-not bit-vector t))

(defun andc1! (bit-vector-1 bit-vector-2)
  (bit-andc1 bit-vector-1 bit-vector-2 t))

(defun andc2! (bit-vector-1 bit-vector-2)
  (bit-andc2 bit-vector-1 bit-vector-2 t))

(defun orc1! (bit-vector-1 bit-vector-2)
  (bit-orc1 bit-vector-1 bit-vector-2 t))

(defun orc2! (bit-vector-1 bit-vector-2)
  (bit-orc2 bit-vector-1 bit-vector-2 t))

(declaim (inline and-into! nand-into! or-into! nor-into! xor-into! xnor-into! not-into! andc1-into! andc2-into! orc1-into! orc2-into!))

(defun and-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-and bit-vector-1 bit-vector-2 result-bit-vector))

(defun nand-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-nand bit-vector-1 bit-vector-2 result-bit-vector))

(defun or-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-ior bit-vector-1 bit-vector-2 result-bit-vector))

(defun nor-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-nor bit-vector-1 bit-vector-2 result-bit-vector))

(defun xor-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-xor bit-vector-1 bit-vector-2 result-bit-vector))

(defun xnor-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-eqv bit-vector-1 bit-vector-2 result-bit-vector))

(defun not-into! (bit-vector result-bit-vector)
  (bit-not bit-vector result-bit-vector))

(defun andc1-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-andc1 bit-vector-1 bit-vector-2 result-bit-vector))

(defun andc2-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-andc2 bit-vector-1 bit-vector-2 result-bit-vector))

(defun orc1-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-orc1 bit-vector-1 bit-vector-2 result-bit-vector))

(defun orc2-into! (bit-vector-1 bit-vector-2 result-bit-vector)
  (bit-orc2 bit-vector-1 bit-vector-2 result-bit-vector))

;; Normal access

(defun make (dimension &key initial-contents initial-element adjustable fill-pointer displaced-to displaced-index-offset)
  "Make a bit vector"
  (flet ((compute-arguments ()
	   (let ((result (list 'bit :element-type dimension)))
	     (flet ((maybe-put! (key value)
		      (when value
			(push key result)
			(push value result))))
	       (maybe-put! :initial-element initial-element)
	       (maybe-put! :initial-contents initial-contents)
	       (maybe-put! :adjustable adjustable)
	       (maybe-put! :fill-pointer fill-pointer)
	       (maybe-put! :displaced-to displaced-to)
	       (maybe-put! :displaced-index-offset displaced-index-offset))
	     (nreverse result))))
    (apply #'make-array (compute-arguments))))

(defun get (bit-vector index)
  (bit bit-vector index))

(defsetf get (bit-vector index) (value)
  `(setf (bit ,bit-vector ,index) ,value))

(defun put! (bit-vector index value)
  (setf (get bit-vector index) value))

(defun concatenate (&rest sequences)
  (apply #'cl:concatenate 'vector sequences))

;; Generic access

