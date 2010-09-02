(in-package #:std.collection)

(defgeneric get (container key)
  (:documentation "Generic access method"))

; TODO: Should this be called set!  ?
; TODO: Should this work on values as well?
(defgeneric set (container key value)
  (:documentation "Generic set method"))

(defgeneric copy (container)
  (:documentation "Generically copy container"))

(std.base:defun-alias 'delete 'remove!)