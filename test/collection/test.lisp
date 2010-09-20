(in-package #:standard-test.collection)

(defun run-all-tests ()
  (run-tests :suite 'standard-collection-test))

(defun %test-get (get collection index expected)
  (ensure-same expected (funcall get collection index)))

(defun %test-put! (get put! collection index old new)
  (ensure-same old (funcall get collection index))
  (funcall put! collection index new)
  (ensure-same new (funcall get collection index)))

(defun %test-copy (copy collection)
  (let ((result (funcall copy collection))
	(*lift-equality-test* 'equalp))
    (ensure-same result collection)
    (ensure-null (eq result collection))))

(defun %test-reduce (reduce collection f expected from-end)
  (let ((*lift-equality-test* 'equalp))
    (ensure-same expected (funcall reduce f collection :from-end from-end))))

(defun %test-reduce-with-i-v (reduce collection f expected from-end initial-value)
  (let ((*lift-equality-test* 'equalp))
    (ensure-same expected (funcall reduce f collection :from-end from-end :initial-value initial-value))))

(defun %test-count (count collection item expected)
  (ensure-same expected (funcall count item collection)))

(defun %test-count-if (count collection item expected)
  (ensure-same expected (funcall count (lambda (i) (eql i item)) collection)))

(defun %test-count-if-not (count collection item expected)
  (ensure-same expected (funcall count (lambda (i) (not (eql i item))) collection)))

(defun %test-find (find collection item)
  (ensure-same item (funcall find item collection)))

(defun %test-find-if (find collection item)
  (ensure-same item (funcall find (lambda (i) (eql i item)) collection)))

(defun %test-find-if-not (find collection item)
  (ensure-same item (funcall find (lambda (i) (not (eql i item))) collection)))

(defun %test-remove^ (find remove^ collection item)
  (ensure-same item (funcall find item collection))
  (let ((result (funcall remove^ item collection)))
    (ensure-null (funcall find item result))
    (ensure-null (funcall find item collection)))) ; NOTE: This is not necassarily true.  E.g. if you use a vector and remove the last element, the vector will probably be unchanged

(defun %test-remove (find remove collection item)
  (ensure-same item (funcall find item collection))
  (let ((result (funcall remove item collection)))
    (ensure-null (funcall find item result))
    (ensure-same item (funcall find item collection))))

(defun %test-remove-if^ (find remove^ collection item)
  (let ((f (lambda (i) (eql i item))))
    (ensure-same item (funcall find f collection))
    (let ((result (funcall remove^ f collection)))
      (ensure-null (funcall find f result))
      (ensure-null (funcall find f collection))))) ; NOTE: This is not necassarily true.  E.g. if you use a vector and remove the last element, the vector will probably be unchanged

(defun %test-remove-if (find remove collection item)
  (let ((f (lambda (i) (eql i item))))
    (ensure-same item (funcall find f collection))
    (let ((result (funcall remove f collection)))
      (ensure-null (funcall find f result))
      (ensure-same item (funcall find f collection)))))

(defun %test-remove-if-not^ (find remove^ collection item)
  (let ((f (lambda (i) (not (eql i item)))))
    (ensure-same item (funcall find f collection))
    (let ((result (funcall remove^ f collection)))
      (ensure-null (funcall find f result))
      (ensure-null (funcall find f collection))))) ; NOTE: This is not necassarily true.  E.g. if you use a vector and remove the last element, the vector will probably be unchanged

(defun %test-remove-if-not (find remove collection item)
  (let ((f (lambda (i) (not (eql i item)))))
    (ensure-same item (funcall find f collection))
    (let ((result (funcall remove f collection)))
      (ensure-null (funcall find f result))
      (ensure-same item (funcall find f collection)))))

(defmacro add-collection-tests (name local-test-fun target-test-funs extra-args collections default-args)
  (flet ((as-symbol (&rest args)
	   (intern (string-upcase (apply #'format nil args)))))
    `(flet ((test (collection ,@extra-args)
	      (,local-test-fun ,@target-test-funs collection ,@extra-args)))
       ,@(loop for c in collections
	    collect (let (cc args)
		      (if (atom c)
			  (setf
			   cc c
			   args default-args)
			  (setf
			   cc (first c)
			   args (rest c)))
		      `(addtest ,(as-symbol "test-~a-~a" name cc)
			(test ,(as-symbol "-~a-" cc) ,@args)))))))

(deftestsuite standard-collection-test ()
  (-list- -array- -vector- -buffer- -string- -hash- -set-)
  (:setup
   (setf -list- (list 'a 'b 'c 'a))
   (setf -array- (array:make '(2 2) :initial-contents '((a b) (c a))))
   (setf -vector- (vector:make 4 :adjustable t :initial-contents (list 'a 'b 'c 'a) :fill-pointer 4))
   (setf -buffer- (vector 'a 'b 'c 'a))
   (setf -string- (string:copy "abca"))
   (setf -hash- (hash:copy #{1 a, 2 b, 3 c, 4 a}))
   (setf -set- (set:copy #[a b c]))))

(add-collection-tests
 get
 %test-get (#'std:get) (index expected)
 (list (array (list 0 1) 'b)  vector buffer (string 1 #\b) (hash 2 'b)) (1 'b))

(add-collection-tests
 put!
 %test-put! (#'std:get #'std:put!) (index old new)
(list (array (list 0 1) 'b 'z) vector buffer (string 1 #\b #\z) (hash 2 'b 'z)) (1 'b 'z))

(add-collection-tests
 copy
 %test-copy (#'std:copy) ()
 (list array vector buffer string hash set) ())

(flet ((test (collection &optional (fun #'+) (expected 6))
	 (%test-reduce #'std:reduce collection fun expected nil)))
  (addtest test-reduce-list
    (test (list 1 2 3)))
  (addtest test-reduce-array
    (test (array:make '(2 2) :initial-contents '((1 2) (3 0)))))
  (addtest test-reduce-vector
    (test (vector:make 3 :adjustable t :initial-contents (list 1 2 3) :fill-pointer 3)))
  (addtest test-reduce-buffer
    (test (vector 1 2 3)))
  (addtest test-reduce-string
    (test -string- #'list (list (list (list #\a #\b) #\c) #\a)))
  (addtest test-reduce-hash
    (test (hash:copy #{a 1, b 2, c 3})))
  (addtest test-reduce-set
    (test (set:copy #[1 2 3]))))

(flet ((test (collection &optional (expected (list 'a 'b 'c 'a)) (fun #'cons))
	 (%test-reduce-with-i-v #'std:reduce collection fun expected t nil)))
  (addtest test-reduce-from-end-list
    (test -list-))
  (addtest test-reduce-from-end-array
    (test -array-))
  (addtest test-reduce-from-end-vector
    (test -vector-))
  (addtest test-reduce-from-end-buffer
    (test -buffer-))
  (addtest test-reduce-from-end-string
    (test -string- (list #\a #\b #\c #\a)))
  (addtest test-reduce-from-end-hash
    (test -hash-))
  (addtest test-reduce-from-end-set
    (test -set-)))

(add-collection-tests
 count
 %test-count (#'std:count) (item expected)
 (list array vector buffer (string #\a 2) hash (set 'a 1)) ('a 2))

(add-collection-tests
 count-if
 %test-count-if (#'std:count-if) (item expected)
 (list array vector buffer (string #\a 2) hash (set 'a 1)) ('a 2))

(add-collection-tests
 count-if-not
 %test-count-if-not (#'std:count-if-not) (item expected)
 (list array vector buffer (string #\a 2) hash (set 'a 1)) ('a 2))

(add-collection-tests
 find
 %test-find (#'std:find) (item)
 (list array vector buffer (string #\b) hash set) ('b))

(add-collection-tests
 find-if
 %test-find-if (#'std:find-if) (item)
 (list array vector buffer (string #\b) hash set) ('b))

(add-collection-tests
 find-if-not
 %test-find-if-not (#'std:find-if-not) (item)
 (list array vector buffer (string #\b) hash set) ('b))

(add-collection-tests
 remove^
 %test-remove^ (#'std:find #'std:remove^) (item)
 (list vector buffer (string #\b)) ('b))

(add-collection-tests
 remove
 %test-remove (#'std:find #'std:remove) (item)
 (list vector buffer (string #\b)) ('b))

(add-collection-tests
 remove-if^
 %test-remove-if^ (#'std:find-if #'std:remove-if^) (item)
 (list vector buffer (string #\b)) ('b))

(add-collection-tests
 remove-if
 %test-remove-if (#'std:find-if #'std:remove-if) (item)
 (list vector buffer (string #\b)) ('b))

(add-collection-tests
 remove-if-not^
 %test-remove-if-not^ (#'std:find-if-not #'std:remove-if-not^) (item)
 (list vector buffer (string #\b)) ('b))

(add-collection-tests
 remove-if-not
 %test-remove-if-not (#'std:find-if-not #'std:remove-if-not) (item)
 (list vector buffer (string #\b)) ('b))

(deftestsuite standard-collection-buffer-test (standard-collection-test)
  ())

(addtest test-get
  (%test-get #'buffer:get -buffer- 1 'b))

(addtest test-put!
  (%test-put! #'buffer:get #'buffer:put! -buffer- 1 'b 'z))

(addtest test-copy
  (%test-copy #'buffer:copy -buffer-))

(addtest test-remove^
  (%test-remove^ #'buffer:find #'buffer:remove^ -buffer- 'b))

(addtest test-remove
  (%test-remove #'buffer:find #'buffer:remove -buffer- 'b))
