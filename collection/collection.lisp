(in-package #:std.collection)

(define-condition invalid-operation-error (simple-error)
  ())

(defun invalid-operation-error (method type)
  (error 'invalid-operation-error :format-control "operation ~a not supported for type ~a" :format-arguments (list method (type-of type))))

(macrolet ((%defgeneric (name arguments &rest options)
	     (flet ((special-name? (name)
		      (or
		       (eq name '&key)
		       (eq name '&optional)
		       (eq name 'collection))))
	       `(defgeneric ,name ,arguments
		  ,@options
		  (:method ,arguments
		    (declare (ignore ,@(cl:remove-if #'special-name? arguments)))
		    (invalid-operation-error ',name collection))))))

  (%defgeneric get (collection key)
    (:documentation "Get value for key"))

  (%defgeneric put! (collection key value)
    (:documentation "Put value at key"))

  (%defgeneric length (collection)
    (:documentation "Length of COLLECTION"))

  (%defgeneric position (item collection &key from-end start end key test)
    (:documentation "Position of ITEM within COLLECTION"))

  (%defgeneric position-if (predicate collection &key from-end start end key)
    (:documentation "Position of ITEM within COLLECTION"))

  (%defgeneric copy (collection &optional start end)
    (:documentation "Returns a copy of COLLECTION from the begginging of the collection or START until the end of the collection or END if provided"))

  (%defgeneric copy^ (collection &optional start end)
    (:documentation "Destructive version of subset"))

  (%defgeneric split (delimiter collection &key key from-end start end count test remove-empty)
    (:documentation "Split COLLECTION into subcollections based on DELIMITER"))

  (%defgeneric split-if (predicate collection &key key from-end start end count remove-empty)
    (:documentation "Split COLLECTION into subcollections based on PREDICATE"))

  (%defgeneric count (item collection &key from-end start end key test)
    (:documentation "How many times item occurs in collection"))

  (%defgeneric count-if (predicate collection &key from-end start end key)
    (:documentation "How many times predicate is statisfied in collection"))

  (%defgeneric reduce (function collection &key key from-end start end initial-value)
    (:documentation "Reduce function accross collection"))

  (%defgeneric find (item collection &key from-end start end key test)
    (:documentation "Find item in collection"))

  (%defgeneric find-if (predicate collection &key from-end start end key)
    (:documentation "Find first matching item in collection"))

  (%defgeneric remove (item collection &key from-end test start end count key)
    (:documentation "Remove item from collection"))

  (%defgeneric remove-if (predicate collection &key from-end start end count key)
    (:documentation "Remove items from collection for which predicate returns t"))

  (%defgeneric remove^ (item collection &key from-end test start end count key)
    (:documentation "Destructive form of remove"))

  (%defgeneric remove-if^ (predicate collection &key from-end start end count key)
    (:documentation "Destructive form of remove-if"))

  (%defgeneric filter (predicate collection &key from-end start end count key)
    (:documentation "Filters items from collection for which predicate returns nil"))

  (%defgeneric filter^ (predicate collection &key from-end start end count key)
    (:documentation "Destructive form of filter"))

  (%defgeneric remove-duplicates (collection &key test start end from-end key)
    (:documentation "Remove duplicates from collection"))

  (%defgeneric remove-duplicates^ (collection &key test start end from-end key)
    (:documentation "Destructive form of remove-duplicates"))

  (%defgeneric reverse (collection)
    (:documentation "Reverse the contents of a collection"))

  (%defgeneric reverse^ (collection)
    (:documentation "Reverse the contents of a collection in place (destructive)"))

  (%defgeneric fill (collection item &key start end)
    (:documentation "Replace specified elements of COLLECTION with ITEM"))

  (%defgeneric substitute (new old collection &key from-end test start count end key)
    (:documentation "Substitute old item for new"))

  (%defgeneric substitute-if (new predicate collection &key from-end start count end key)
    (:documentation "Substitute matching items for new"))

  (%defgeneric substitute! (new old collection &key from-end test start count end key)
    (:documentation "Substitute old item for new"))

  (%defgeneric substitute-if! (new predicate collection &key from-end start count end key)
    (:documentation "Substitute matching items for new in place"))

  (%defgeneric sort (collection predicate &key key)
    (:documentation "Sort the contents of a collection"))

  (%defgeneric sort^ (collection predicate &key key)
    (:documentation "Sort the contents of a collection destructively"))

  (%defgeneric stable-sort (collection predicate &key key)
    (:documentation "Stable version of sort function"))

  (%defgeneric stable-sort^ (collection predicate &key key)
    (:documentation "Destructive version of stable-sort")))

(defgeneric merge (output-spec collection1 collection2 predicate &key key)
  (:documentation "Merge 2 sorted collections"))

(defgeneric merge^ (output-spec collection1 collection2 predicate &key key)
  (:documentation "Merge 2 sorted collections destrucively"))