(in-package #:standard-test.collection)

(defun run-all-tests ()
  (run-tests :suite 'standard-collection-test))

(deftestsuite standard-collection-test ()
  (-list- -array- -vector- -buffer- -string- -hash- -set-)
  (:setup
   (setf -list- (list 'a 'b 'c))
   (setf -array- (array:make '(2 2) :initial-contents '((a b) (c d))))
   (setf -vector- (vector:make 3 :adjustable t :initial-contents (list 'a 'b 'c) :fill-pointer 3))
   (setf -buffer- (vector 'a 'b 'c))
   (setf -string- (string:copy "abc"))
   (setf -hash- (hash:copy #{1 a, 2 b, 3 c}))
   (setf -set- (set:copy #[a b c])))
  (:function
   (%test-get (get collection index expected)
	      (ensure-same expected (funcall get collection index))))
  (:function
   (%test-put! (get put! collection index old new)
	       (ensure-same old (funcall get collection index))
	       (funcall put! collection index new)
	       (ensure-same new (funcall get collection index))))
  (:function
   (%test-copy (copy collection)
	       (let ((result (funcall copy collection))
		     (*lift-equality-test* 'equalp))
		 (ensure-same result collection)
		 (ensure-null (eq result collection)))))
  (:function
   (%test-reduce (reduce collection f expected from-end)
		 (let ((*lift-equality-test* 'equalp))
		   (ensure-same expected (funcall reduce f collection :from-end from-end)))))
  (:function
   (%test-reduce-with-i-v (reduce collection f expected from-end initial-value)
		 (let ((*lift-equality-test* 'equalp))
		   (ensure-same expected (funcall reduce f collection :from-end from-end :initial-value initial-value)))))
  (:function
   (%test-find (find item collection)
	       (ensure-same item (funcall find item collection))))
  (:function
   (%test-find-if (find item collection)
		  (ensure-same item (funcall find (lambda (i) (eql i item)) collection))))
  (:function
   (%test-find-if-not (find item collection)
		      (ensure-same item (funcall find (lambda (i) (not (eql i item))) collection))))
  (:function
   (%test-remove^ (find remove^ item collection)
		 (ensure-same item (funcall find item collection))
		 (let ((result (funcall remove^ item collection)))
		   (ensure-null (funcall find item result))
		   (ensure-null (funcall find item collection))))) ; NOTE: This is not necassarily true.  E.g. if you use a vector and remove the last element, the vector will probably be unchanged
  (:function
   (%test-remove (find remove item collection)
		 (ensure-same item (funcall find item collection))
		 (let ((result (funcall remove item collection)))
		   (ensure-null (funcall find item result))
		   (ensure-same item (funcall find item collection))))))

(addtest test-get
  (flet ((test (collection index expected)
	     (%test-get #'collection:get collection index expected)))
    (test -list- 1 'b)
    (test -vector- 1 'b)
    (test -buffer- 1 'b)
    (test -string- 1 #\b)
    (test -hash- 2 'b)))

(addtest test-put!
  (flet ((test (collection index old new)
	   (%test-put! #'collection:get #'collection:put! collection index old new)))
    (test -list- 1 'b 'z)
    (test -vector- 1 'b 'z)
    (test -buffer- 1 'b 'z)
    (test -string- 1 #\b #\z)
    (test -hash- 2 'b 'z)))

(addtest test-copy
  (flet ((test (collection)
	   (%test-copy #'collection:copy collection)))
    (test -list-)
    (test -array-)
    (test -vector-)
    (test -buffer-)
    (test -string-)
    (test -hash-)
    (test -set-)))

(addtest test-reduce
  (flet ((test (collection &optional (fun #'+) (expected 6))
	   (%test-reduce #'collection:reduce collection fun expected nil)))
    (test (list 1 2 3))
    (test (array:make '(2 2) :initial-contents '((1 2) (3 0))))
    (test (vector:make 3 :adjustable t :initial-contents (list 1 2 3) :fill-pointer 3))
    (test (vector 1 2 3))
    (test -string- #'list (list (list #\a #\b) #\c))
    (test (hash:copy #{a 1, b 2, c 3}))
    (test (set:copy #[1 2 3]))))

(addtest test-reduce-from-end
  (flet ((test (collection &optional (expected (list 'a 'b 'c)) (fun #'cons))
	   (%test-reduce-with-i-v #'collection:reduce collection fun expected t nil)))
    (test -list-)
    (test -array- (list 'a 'b 'c 'd))
    (test -vector-)
    (test -buffer-)
    (test -string- (list #\a #\b #\c))
    (test -hash-)
    (test -set-)))

(addtest test-find
  (flet ((test (collection item)
	   (%test-find #'collection:find item collection)))
    (test -list- 'b)
    (test -array- 'b)
    (test -vector- 'b)
    (test -buffer- 'b)
    (test -string- #\b)
    (test -hash- 'b)
    (test -set- 'b)))

(addtest test-find-if
  (flet ((test (collection item)
	   (%test-find-if #'collection:find-if item collection)))
    (test -list- 'b)
    (test -array- 'b)
    (test -vector- 'b)
    (test -buffer- 'b)
    (test -string- #\b)
    (test -hash- 'b)
    (test -set- 'b)))

(addtest test-find-if-not
  (flet ((test (collection item)
	   (%test-find-if-not #'collection:find-if-not item collection)))
    (test -list- 'b)
    (test -array- 'b)
    (test -vector- 'b)
    (test -buffer- 'b)
    (test -string- #\b)
    (test -hash- 'b)
    (test -set- 'b)))

(addtest test-remove^
  (flet ((test (collection item)
	   (%test-remove^ #'collection:find #'collection:remove^ item collection)))
    (test -list- 'b)
    (test -vector- 'b)
    (test -buffer- 'b)
    (test -string- #\b)))

(addtest test-remove
  (flet ((test (collection item)
	   (%test-remove #'collection:find #'collection:remove item collection)))
    (test -list- 'b)
    (test -vector- 'b)
    (test -buffer- 'b)
    (test -string- #\b)))    

(deftestsuite standard-collection-buffer-test (standard-collection-test)
  ())

(addtest test-get
  (%test-get #'buffer:get -buffer- 1 'b))

(addtest test-put!
  (%test-put! #'buffer:get #'buffer:put! -buffer- 1 'b 'z))

(addtest test-copy
  (%test-copy #'buffer:copy -buffer-))

(addtest test-remove^
  (%test-remove^ #'buffer:find #'buffer:remove^ 'b -buffer-))

(addtest test-remove
  (%test-remove #'buffer:find #'buffer:remove 'b -buffer-))
