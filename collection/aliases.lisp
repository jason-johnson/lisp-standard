(in-package #:std.collection)

;; TODO: Does this still need to come at the end?

;; List

;; NOTE: Nothing, it all comes from CL

;; Sequence functions

(defun-alias 'cl:map 'map-to)

(defun-alias 'delete 'remove!)
(defun-alias 'delete-if 'remove-if!)
(defun-alias 'delete-if-not 'remove-if-not!)
(defun-alias 'delete-duplicates 'remove-duplicates!)
(defun-alias 'nreverse 'reverse!)
(defun-alias 'nsubstitute 'substitute!)
(defun-alias 'nsubstitute-if 'substitute-if!)
(defun-alias 'nsubstitute-if-not 'substitute-if-not!)
(defun-alias 'nconc 'append!)
