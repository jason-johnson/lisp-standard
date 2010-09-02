(in-package #:std.base)

(declaim (optimize (debug 3) (speed 0) (space 0) (compilation-speed 0)))

;; Package management

(defmacro def-metapackage (name packages &optional nicknames)
  "Defines a package that simply consists of using and exporting all external symbols of the given packages (symbol preference is chosen based on order of appearance)"
  (let ((shadow-imports (make-hash-table))
	(exported-symbols (make-hash-table :test 'equal)))
    (flet ((insert (sym package)
	     (let ((old-package (gethash sym exported-symbols)))
	       (if old-package
		   (pushnew sym (gethash old-package shadow-imports))
		   (setf (gethash sym exported-symbols) package)))))
      (dolist (package packages)
	(loop for sym being the external-symbols of package
	   do (insert (symbol-name sym) package))))
    `(defpackage ,name
       (:use ,@packages)
       (:nicknames ,@nicknames)
       ,@(loop for pkg being the hash-keys of shadow-imports
	    for syms being the hash-values of shadow-imports
	    collect `(:shadowing-import-from ,pkg ,@syms))
       (:export ,@(loop for sym being the hash-keys of exported-symbols
		       collect sym)))))

(defmacro def-metapackage-compliment (name source-packages compliment-packages &optional exception-symbols nicknames)
  "Defines a package exporting the result of applying a set compliment to all exported symbols from source-packages with all exported symbols from compliment-packages.  If specified exception-symbols will also be excluded."
  (let ((exported-symbols (make-hash-table :test 'equal)))
    (flet ((to-list (thing)
	     (etypecase thing
	       (list thing)
	       (symbol (if (boundp thing)
			   (eval thing)
			   (error "~a(~S) received where list expected" (type-of thing) thing)))))
	   (add (symbol)
	     (setf (gethash (symbol-name symbol) exported-symbols) t))
	   (del (symbol)
	     (remhash (symbol-name symbol) exported-symbols)))
      (dolist (package (to-list source-packages))
	(do-external-symbols (sym package)
	  (add sym)))
      (dolist (package (to-list compliment-packages))
	(do-external-symbols (sym package)
	  (del sym)))
      (dolist (sym (to-list exception-symbols))
	(del sym)))
    `(defpackage ,name
       (:use ,@source-packages)
       (:nicknames ,@nicknames)
       (:export ,@(loop for sym being the hash-keys of exported-symbols collect sym)))))

;; User functions/macros

(defmacro set! (&rest args)
  `(setf ,@args))

(defmacro defset! (access-fn &rest rest)
  `(defsetf ,access-fn ,@rest))

(defmacro inc! (place &optional (delta 1))
  `(incf ,place ,delta))

(defmacro dec! (place &optional (delta 1))
  `(decf ,place ,delta))

;; TODO: Look into adding the ability to add documentation to destination
;; TODO: Update: documentation lives with the function, not the symbol so changing the documentation of the alias changes it for both. :(
(defmacro defun-alias (source destination &optional documentation) ; TODO: if this is null then use the doc from the source function
  (declare (ignore documentation))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-function ,destination) (symbol-function ,source))))

;; Generic functions

(defgeneric copy (object)
  (:documentation "Get a copy of object"))
