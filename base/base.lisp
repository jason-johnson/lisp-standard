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

(defmacro rotate! (&rest args)
  `(rotatef ,@args))

;; TODO: Look into adding the ability to add documentation to destination
;; TODO: Update: documentation lives with the function, not the symbol so changing the documentation of the alias changes it for both. :(
;; TODO: UPDATE: This macro could be changed to declare the "alias" inline and then just define it as a function that calls the aliased function.
;; TODO: UPDATE: That should acheive all our goals here and not have the problems of the current approach
(defmacro defun-alias (source destination &optional documentation) ; TODO: if this is null then use the doc from the source function
  (declare (ignore documentation))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-function ,destination) (symbol-function ,source))))

;; Looping

;; NOTE: Do is used in the next two because that enables #'go and #'return to be used in their bodies.  This would not be the case with loop
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

;; These next two functions should probably be moved to another package at some point

(defun compose (&rest functions)
  (setf functions (nreverse functions))
  (lambda (&rest args)
    (reduce (lambda (val f) (funcall f val)) (rest functions) :initial-value (apply (first functions) args))))

(let (iterate-value)
  (defun iterate (function initial-value)
    "Returns a function that when called will return the results of applying FUNCTION to the last result of the call (starting value will be INITIAL-VALUE)"
    (setf iterate-value initial-value)
    (lambda ()
      (prog1
	  iterate-value
	(setf iterate-value (funcall function iterate-value))))))

;; Macro helpers

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
		,@body)))))

;; Generic functions

(defgeneric copy (object)
  (:documentation "Get a copy of object"))

(defmethod copy (object)
  object)				; Immediate objects are immutable and don't need to be copied

(defgeneric coerce (object output-spec)
  (:documentation "Convert OBJECT into OUTPUT-SPEC"))

(defmethod coerce (object output-spec)
  (cl:coerce object output-spec))