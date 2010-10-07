(in-package #:standard-test.collection)

(defun run-all-tests ()
  (run-tests :suite 'standard-collection-test))

(defun make-vector (&rest args)
  (let ((length (cl:length args)))
    (vector:make length :adjustable t :initial-contents args :fill-pointer length)))

(defun %test-get (get collection index expected)
  (ensure-same expected (funcall get collection index)))

(defun %test-put! (get put! collection index old new)
  (ensure-same old (funcall get collection index))
  (funcall put! collection index new)
  (ensure-same new (funcall get collection index)))

(defun %test-position (position collection item expected &optional from-end)
  (ensure-same expected (funcall position item collection :from-end from-end)))

(defun %test-base-copy (copy collection)
  (let ((result (funcall copy collection))
	(*lift-equality-test* 'equalp))
    (ensure-same result collection)
    (ensure-null (eq result collection))))

(defun %test-collection-copy (copy collection expected start end)
  (let ((result (funcall copy collection start end))
	(c (std:copy collection)))
    (ensure-null (equalp result collection))
    (ensure (equalp result expected))
    (ensure-null (eq result collection))
    (ensure (equalp collection c))))

(defun %test-collection-copy^ (copy collection expected start end)
  (let ((c (std:copy collection))
	(result (funcall copy collection start end)))
    (ensure-null (equalp result collection))
    (ensure (equalp result expected))
    (ensure-null (eq result collection))
    (ensure-null (equalp collection c))))

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

(defun %test-fill (count fill collection item)
  (ensure-same 0 (funcall count item collection))
  (let ((result (funcall fill collection item :end 2)))
    (ensure-same 2 (funcall count item result))
    (ensure (equalp result collection))))

(defun %test-substitute (substitute count collection old new)
  (let ((c (funcall count old collection))
	(result (funcall substitute new old collection)))
    (ensure (< 0 c))
    (ensure-same c (funcall count old collection))
    (ensure-same 0 (funcall count old result))
    (ensure-same c (funcall count new result))))

(defun %test-substitute! (substitute! count collection old new)
  (let ((c (funcall count old collection))
	(result (funcall substitute! new old collection)))
    (ensure (< 0 c))
    (ensure-same 0 (funcall count old collection))
    (ensure-same c (funcall count new collection))
    (ensure (equalp result collection))))

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

(defmacro add-collection-tests-with-template (name local-test-fun target-test-funs extra-args template collections)
  (flet ((template (tp mt)
	   `(macrolet ((type (&rest args)
			 `(list ',',tp ,@args))
		       (make-type (&rest args)
			 `(list ',',mt ,@args)))
	      ,@template)))
    `(add-collection-tests
      ,name
      ,local-test-fun ,target-test-funs ,extra-args
      ,(loop for c in collections
	   collect
	     (if (and
		  (not (atom c))
		  (eq (first c) 'template))
		 (eval (apply #'template (rest c)))
		 c)))))

(deftestsuite standard-collection-test ()
  (-list- -array- -vector- -buffer- -string- -hash- -set-)
  (:setup
   (setf -list- (list 'a 'b 'c 'a))
   (setf -array- (array:make '(2 2) :initial-contents '((a b) (c a))))
   (setf -vector- (make-vector 'a 'b 'c 'a))
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
 base-copy
 %test-base-copy (#'std:copy) ()
 (list array vector buffer string hash set))

(add-collection-tests
 position
 %test-position (#'std:position) ((item 'a) (expected 0))
 (list array vector buffer (string #\a) hash set))

(add-collection-tests
 position-from-end
 %test-position (#'std:position) ((item 'a) (expected 3) (from-end t))
 (list array vector buffer (string #\a) hash set))

(add-collection-tests
 collection-copy
 %test-collection-copy (#'std.collection:copy) (expected (start 1) (end 3))
 ((list (list 'b 'c))
  (vector (make-vector 'b 'c))
  (buffer (vector 'b 'c))
  (string "bc")))

(add-collection-tests
 collection-copy^
 %test-collection-copy^ (#'std.collection:copy^) (expected (start 1) (end 3))
 ((list (list 'b 'c))
  (vector (make-vector 'b 'c))
  (buffer (vector 'b 'c))
  (string "bc")))

(add-collection-tests
 reduce
 %test-reduce (#'std:reduce) ((fun #'+) (expected 6) from-end)
 (
  ((list (list 1 2 3)))
  ((array (array:make '(2 2) :initial-contents '((1 2) (3 0)))))
  ((vector (make-vector 1 2 3)))
  ((buffer (vector 1 2 3)))
  (string #'list (list (list (list #\a #\b) #\c) #\a))
  ((hash (hash:copy #{a 1, b 2, c 3})))
  ((set (set:copy #[1 2 3])))))

(add-collection-tests
 reduce-from-end
 %test-reduce-with-i-v (#'std:reduce) ((fun #'cons) (expected (list 'a 'b 'c 'a)) (from-end t) initial-value)
 (list array vector buffer (string #'cons (list #\a #\b #\c #\a)) hash))

;; NOTE: Specialized for set since we can't guess the order the set will be in.  But this working proves that :from-end t causes the cons to work correctly
(addtest test-reduce-from-end-set
  (let ((expected (list 'a 'b 'c))
	(result (std:reduce #'cons -set- :initial-value nil :from-end t)))
    (ensure-same (std:length expected) (std:length result))
    (dolist (v expected)
      (ensure (std:find v result)))))

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

;; NOTE: Hash makes no sense for remove-duplicates because keys are also relevant (how do you know which to keep?).  Array makes no sense because how would you truncate on subarray but not the other at that level
(add-collection-tests
 remove-duplicates
 %test-remove-duplicates (#'std:count #'std:remove-duplicates) ((item 'a) (starting 2))
 (list vector buffer (string #\a 2) (set 'a 1)))

(add-collection-tests
 remove-duplicates^
 %test-remove-duplicates^ (#'std:count #'std:remove-duplicates^) ((item 'a) (starting 2))
 (((list (list 'a 'b 'a 'a 'c 'a)) 'a 4) vector buffer (string #\a 2) (set 'a 1)))

;; NOTE: Hash and Set can't be reversed because they're not ordered
(add-collection-tests
 reverse
 %test-reverse (#'std:reverse) ()
 (list array vector buffer string))

(add-collection-tests
 reverse^
 %test-reverse^ (#'std:reverse^) ()
 (list array vector buffer string))

(add-collection-tests
 fill
 %test-fill (#'std:count #'std:fill) ((item 'z))
 (list array vector buffer (string #\z)))

(add-collection-tests
 substitute
 %test-substitute (#'std:substitute #'std:count) ((old 'a) (new 'z))
 (list array vector buffer (string #\a #\z) hash set))

(add-collection-tests
 substitute!
 %test-substitute! (#'std:substitute! #'std:count) ((old 'a) (new 'z))
 (list array vector buffer (string #\a #\z) hash set))

(deftestsuite standard-collection-buffer-test (standard-collection-test)
  ())

(addtest test-get
  (%test-get #'buffer:get -buffer- 1 'b))

(addtest test-put!
  (%test-put! #'buffer:get #'buffer:put! -buffer- 1 'b 'z))

(addtest test-copy
  (%test-base-copy #'buffer:copy -buffer-))

(addtest test-remove^
  (%test-remove^ #'buffer:find #'buffer:remove^ -buffer- 'b))

(addtest test-remove
  (%test-remove #'buffer:find #'buffer:remove -buffer- 'b))