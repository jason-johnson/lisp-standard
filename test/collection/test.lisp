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

(defun %test-remove-duplicates (count remove collection item starting)
  (ensure-same starting (funcall count item collection))
  (let ((result (funcall remove collection)))
    (ensure-same 1 (funcall count item result))
    (ensure-same starting (funcall count item collection))))

(defun %test-remove-duplicates^ (count remove^ collection item starting)
  (ensure-same starting (funcall count item collection))
  (let ((original (std:copy collection))
	(result (funcall remove^ collection)))
    (ensure-same 1 (funcall count item result))
    (ensure (not (equalp original collection)))))

(defun %test-reverse (reverse collection)
  (let ((result (funcall reverse collection)))
    (ensure-null (equalp result collection))
    (setf result (funcall reverse result))
    (ensure (equalp result collection))))

(defun %test-reverse^ (reverse^ collection)
  (let ((original (std:copy collection))
	(result (funcall reverse^ collection)))
    (ensure-null (equalp result original))
    (ensure-null (equalp original collection))
    (setf result (funcall reverse^ result))
    (ensure (equalp result original))))

(defmacro add-collection-tests (name local-test-fun target-test-funs extra-args collections)
  (flet ((strip-defaults (args)
	   (mapcar
	    (lambda (arg)
	      (if (atom arg)
		  arg
		  (first arg)))
	    args))
	 (as-symbol (&rest args)
	   (intern (string-upcase (apply #'format nil args)))))
    `(flet ((test (collection &optional ,@extra-args)
	      (,local-test-fun ,@target-test-funs collection ,@(strip-defaults extra-args))))
       ,@(loop for c in collections
	    collect (let (cname args collection)
		      (if (atom c)
			  (setf
			   cname c)
			  (setf
			   cname (first c)
			   args (rest c)))
		      (if (atom cname)
			  (setf collection (as-symbol "-~a-" cname))
			  (setf
			   collection (second cname)
			   cname (first cname)))
		      `(addtest ,(as-symbol "test-~a-~a" name cname)
			(test ,collection ,@args)))))))

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
 %test-get (#'std:get) ((index 1) (expected 'b))
 (list (array (list 0 1))  vector buffer (string 1 #\b) (hash 2)))

(add-collection-tests
 put!
 %test-put! (#'std:get #'std:put!) ((index 1) (old 'b) (new 'z))
(list (array (list 0 1)) vector buffer (string 1 #\b #\z) (hash 2)))

(add-collection-tests
 copy
 %test-copy (#'std:copy) ()
 (list array vector buffer string hash set))

(add-collection-tests
 reduce
 %test-reduce (#'std:reduce) ((fun #'+) (expected 6) from-end)
 (
  ((list (list 1 2 3)))
  ((array (array:make '(2 2) :initial-contents '((1 2) (3 0)))))
  ((vector (vector:make 3 :adjustable t :initial-contents (list 1 2 3) :fill-pointer 3)))
  ((buffer (vector 1 2 3)))
  (string #'list (list (list (list #\a #\b) #\c) #\a))
  ((hash (hash:copy #{a 1, b 2, c 3})))
  ((set (set:copy #[1 2 3])))))

(add-collection-tests
 reduce-from-end
 %test-reduce-with-i-v (#'std:reduce) ((fun #'cons) (expected (list 'a 'b 'c 'a)) (from-end t) initial-value)
 (list array vector buffer (string #'cons (list #\a #\b #\c #\a)) hash set))

(add-collection-tests
 count
 %test-count (#'std:count) ((item 'a) (expected 2))
 (list array vector buffer (string #\a) hash (set 'a 1)))

(add-collection-tests
 count-if
 %test-count-if (#'std:count-if) ((item 'a) (expected 2))
 (list array vector buffer (string #\a) hash (set 'a 1)))

(add-collection-tests
 count-if-not
 %test-count-if-not (#'std:count-if-not) ((item 'a) (expected 2))
 (list array vector buffer (string #\a) hash (set 'a 1)))

(add-collection-tests
 find
 %test-find (#'std:find) ((item 'b))
 (list array vector buffer (string #\b) hash set))

(add-collection-tests
 find-if
 %test-find-if (#'std:find-if) ((item 'b))
 (list array vector buffer (string #\b) hash set))

(add-collection-tests
 find-if-not
 %test-find-if-not (#'std:find-if-not) ((item 'b))
 (list array vector buffer (string #\b) hash set))

(add-collection-tests
 remove^
 %test-remove^ (#'std:find #'std:remove^) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove
 %test-remove (#'std:find #'std:remove) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove-if^
 %test-remove-if^ (#'std:find-if #'std:remove-if^) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove-if
 %test-remove-if (#'std:find-if #'std:remove-if) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove-if-not^
 %test-remove-if-not^ (#'std:find-if-not #'std:remove-if-not^) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove-if-not
 %test-remove-if-not (#'std:find-if-not #'std:remove-if-not) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove-duplicates
 %test-remove-duplicates (#'std:count #'std:remove-duplicates) ((item 'a) (starting 2))
 (list array vector buffer (string #\a 2) hash (set 'a 1)))

(add-collection-tests
 remove-duplicates^
 %test-remove-duplicates^ (#'std:count #'std:remove-duplicates^) ((item 'a) (starting 2))
 (((list (list 'a 'b 'a 'a 'c 'a)) 'a 4) array vector buffer (string #\a 2) hash (set 'a 1)))

(add-collection-tests
 reverse
 %test-reverse (#'std:reverse) ()
 (list array vector buffer string hash set))

(add-collection-tests
 reverse^
 %test-reverse^ (#'std:reverse^) ()
 (list array vector buffer string hash set))

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