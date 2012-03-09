(in-package #:std.collection.bit-array)

;; Bit-vector specific

(declaim (inline and nand or nor xor xnor not andc1 andc2 orc1 orc2))

(defun and (bit-array-1 bit-array-2)
  (bit-and bit-array-1 bit-array-2))

(defun nand (bit-array-1 bit-array-2)
  (bit-nand bit-array-1 bit-array-2))

(defun or (bit-array-1 bit-array-2)
  (bit-ior bit-array-1 bit-array-2))

(defun nor (bit-array-1 bit-array-2)
  (bit-nor bit-array-1 bit-array-2))

(defun xor (bit-array-1 bit-array-2)
  (bit-xor bit-array-1 bit-array-2))

(defun xnor (bit-array-1 bit-array-2)
  (bit-eqv bit-array-1 bit-array-2))

(defun not (bit-array)
  (bit-not bit-array))

(defun andc1 (bit-array-1 bit-array-2)
  (bit-andc1 bit-array-1 bit-array-2))

(defun andc2 (bit-array-1 bit-array-2)
  (bit-andc2 bit-array-1 bit-array-2))

(defun orc1 (bit-array-1 bit-array-2)
  (bit-orc1 bit-array-1 bit-array-2))

(defun orc2 (bit-array-1 bit-array-2)
  (bit-orc2 bit-array-1 bit-array-2))

(declaim (inline and! nand! or! nor! xor! xnor! not! andc1! andc2! orc1! orc2!))

(defun and! (bit-array-1 bit-array-2)
  (bit-and bit-array-1 bit-array-2 t))

(defun nand! (bit-array-1 bit-array-2)
  (bit-nand bit-array-1 bit-array-2 t))

(defun or! (bit-array-1 bit-array-2)
  (bit-ior bit-array-1 bit-array-2 t))

(defun nor! (bit-array-1 bit-array-2)
  (bit-nor bit-array-1 bit-array-2 t))

(defun xor! (bit-array-1 bit-array-2)
  (bit-xor bit-array-1 bit-array-2 t))

(defun xnor! (bit-array-1 bit-array-2)
  (bit-eqv bit-array-1 bit-array-2 t))

(defun not! (bit-array)
  (bit-not bit-array t))

(defun andc1! (bit-array-1 bit-array-2)
  (bit-andc1 bit-array-1 bit-array-2 t))

(defun andc2! (bit-array-1 bit-array-2)
  (bit-andc2 bit-array-1 bit-array-2 t))

(defun orc1! (bit-array-1 bit-array-2)
  (bit-orc1 bit-array-1 bit-array-2 t))

(defun orc2! (bit-array-1 bit-array-2)
  (bit-orc2 bit-array-1 bit-array-2 t))

(declaim (inline and-into! nand-into! or-into! nor-into! xor-into! xnor-into! not-into! andc1-into! andc2-into! orc1-into! orc2-into!))

(defun and-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-and bit-array-1 bit-array-2 result-bit-array))

(defun nand-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-nand bit-array-1 bit-array-2 result-bit-array))

(defun or-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-ior bit-array-1 bit-array-2 result-bit-array))

(defun nor-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-nor bit-array-1 bit-array-2 result-bit-array))

(defun xor-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-xor bit-array-1 bit-array-2 result-bit-array))

(defun xnor-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-eqv bit-array-1 bit-array-2 result-bit-array))

(defun not-into! (bit-array result-bit-array)
  (bit-not bit-array result-bit-array))

(defun andc1-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-andc1 bit-array-1 bit-array-2 result-bit-array))

(defun andc2-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-andc2 bit-array-1 bit-array-2 result-bit-array))

(defun orc1-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-orc1 bit-array-1 bit-array-2 result-bit-array))

(defun orc2-into! (bit-array-1 bit-array-2 result-bit-array)
  (bit-orc2 bit-array-1 bit-array-2 result-bit-array))

;; Normal access

(defun make (dimension &key initial-contents initial-element adjustable fill-pointer displaced-to displaced-index-offset)
  "Make a bit array"
  (flet ((compute-arguments ()
	   (let ((result (list dimension)))
	     (labels ((put! (key value)
			(push key result)
			(push value result))
		      (maybe-put! (key value)
			(when value
			  (put! key value))))
	       (put! :element-type 'bit)
	       (maybe-put! :initial-element initial-element)
	       (maybe-put! :initial-contents initial-contents)
	       (maybe-put! :adjustable adjustable)
	       (maybe-put! :fill-pointer fill-pointer)
	       (maybe-put! :displaced-to displaced-to)
	       (maybe-put! :displaced-index-offset displaced-index-offset))
	     (nreverse result))))
    (apply #'make-array (compute-arguments))))

;; Generic access