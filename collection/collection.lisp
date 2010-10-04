(in-package #:std.collection)

(defgeneric get (collection key)
  (:documentation "Get value for key"))

(defgeneric put! (collection key value)
  (:documentation "Put value at key"))

(defgeneric length (collection)
  (:documentation "Length of COLLECTION"))

(defgeneric position (item collection &key from-end start end key test test-not)
  (:documentation "Position of ITEM within COLLECTION"))

(defgeneric position-if (predicate collection &key from-end start end key)
  (:documentation "Position of ITEM within COLLECTION"))

(defgeneric position-if-not (predicate collection &key from-end start end key)
  (:documentation "Position of ITEM within COLLECTION"))


(defgeneric split (delimiter collection &key key from-end start end count test test-not remove-empty)
  (:documentation "Split COLLECTION into subcollections based on DELIMITER"))

(defgeneric split-if (predicate collection &key key from-end start end count remove-empty)
  (:documentation "Split COLLECTION into subcollections based on PREDICATE"))

(defgeneric split-if-not (predicate collection &key key from-end start end count remove-empty)
  (:documentation "Split COLLECTION into subcollections base on not satisfying PREDICATE"))

(defgeneric count (item collection &key from-end start end key test test-not)
  (:documentation "How many times item occurs in collection"))

(defgeneric count-if (predicate collection &key from-end start end key)
  (:documentation "How many times predicate is statisfied in collection"))

(defgeneric count-if-not (predicate collection &key from-end start end key)
  (:documentation "How many times predicate is not statisfied in collection"))

(defgeneric reduce (function collection &key key from-end start end initial-value)
  (:documentation "Reduce function accross collection"))

(defgeneric find (item collection &key from-end start end key test test-not)
  (:documentation "Find item in collection"))

(defgeneric find-if (predicate collection &key from-end start end key)
  (:documentation "Find first matching item in collection"))

(defgeneric find-if-not (predicate collection &key from-end start end key)
  (:documentation "Find first non-matching item in collection"))

(defgeneric remove (item collection &key from-end test test-not start end count key)
  (:documentation "Remove item from collection"))

(defgeneric remove^ (item collection &key from-end test test-not start end count key)
  (:documentation "Destructive form of remove"))

(defgeneric remove-if (predicate collection &key from-end start end count key)
  (:documentation "Remove items from collection for which predicate returns t"))

(defgeneric remove-if^ (predicate collection &key from-end start end count key)
  (:documentation "Destructive form of remove-if"))

(defgeneric remove-if-not (predicate collection &key from-end start end count key)
  (:documentation "Remove items from collection for which predicate returns nil"))

(defgeneric remove-if-not^ (predicate collection &key from-end start end count key)
  (:documentation "Destructive form of remove-if-not"))

(defgeneric remove-duplicates (collection &key test test-not start end from-end key)
  (:documentation "Remove duplicates from collection"))

(defgeneric remove-duplicates^ (collection &key test test-not start end from-end key)
  (:documentation "Destructive form of remove-duplicates"))

(defgeneric reverse (collection)
  (:documentation "Reverse the contents of a collection"))

(defgeneric reverse^ (collection)
  (:documentation "Reverse the contents of a collection in place (destructive)"))

(defgeneric fill (collection item &key start end)
  (:documentation "Replace specified elements of COLLECTION with ITEM"))

(defgeneric substitute (new old collection &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute! (new old collection &key from-end test test-not start count end key)
  (:documentation "Substitute old item for new"))

(defgeneric substitute-if (new predicate collection &key from-end start count end key)
  (:documentation "Substitute matching items for new"))

(defgeneric substitute-if! (new predicate collection &key from-end start count end key)
  (:documentation "Substitute matching items for new in place"))

(defgeneric substitute-if-not (new predicate collection &key from-end start count end key)
  (:documentation "Substitute non-matching items for new"))

(defgeneric substitute-if-not! (new predicate collection &key from-end start count end key)
  (:documentation "Substitute non-matching item for new in place"))

(defgeneric append (collection &rest collections)
  (:documentation "Append collections together with collection"))

(defgeneric append! (collection &rest collections)
  (:documentation "Like append, but in place"))

(defgeneric sort (collection predicate &key key)
  (:documentation "Sort the contents of a collection"))

(defgeneric sort^ (collection predicate &key key)
  (:documentation "Sort the contents of a collection destructively"))

(defgeneric stable-sort (collection predicate &key key)
  (:documentation "Stable version of sort function"))

(defgeneric stable-sort^ (collection predicate &key key)
  (:documentation "Destructive version of stable-sort"))

(defgeneric merge (output-spec collection1 collection2 predicate &key key)
  (:documentation "Merge 2 sorted collections"))

(defgeneric merge^ (output-spec collection1 collection2 predicate &key key)
  (:documentation "Merge 2 sorted collections destrucively"))