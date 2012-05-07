(in-package #:standard-test.collection)

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

(defun %test-position-if (position collection item expected &optional from-end)
  (ensure-same expected (funcall position (lambda (i) (eql i item)) collection :from-end from-end)))

(defun %test-position-if-not (position collection item expected &optional from-end)
  (ensure-same expected (funcall position (lambda (i) (not (eql i item))) collection :from-end from-end)))

(defun %test-base-copy (copy collection)
  (let ((result (funcall copy collection))
	(*lift-equality-test* 'equalp))
    (ensure-same result collection)
    (ensure-null (eq result collection))))

(defun %test-length (length collection expected)
  (ensure-same expected (funcall length collection)))

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

(flet ((%test (collection find item action)
	 (ensure-same item (funcall find item collection))
	 (let ((result (funcall action)))
	   (ensure-null (funcall find item result))
	   (ensure-same item (funcall find item collection)))))
  (defun %test-remove (find remove collection item)
    (%test collection find item (lambda () (funcall remove item collection))))
  (defun %test-remove-if (find remove collection item)
    (let ((f (lambda (i) (eql i item))))
      (%test collection find item (lambda () (funcall remove f collection)))))
  (defun %test-remove-if-not (find remove collection item)
    (let ((f (lambda (i) (not (eql i item)))))
      (%test collection find item (lambda () (funcall remove f collection))))))

(flet ((%test (collection find item action)
	 (ensure-same item (funcall find item collection))
	 (let ((result (funcall action)))
	   (ensure-null (funcall find item result))
	   (ensure-null (funcall find item collection))))) ; NOTE: This is not necassarily true.  E.g. if you use a vector and remove the last element, the vector will probably be unchanged
  (defun %test-remove^ (find remove^ collection item)
    (%test collection find item (lambda () (funcall remove^ item collection))))
  (defun %test-remove-if^ (find remove^ collection item)
    (let ((f (lambda (i) (eql i item))))
      (%test collection find item (lambda () (funcall remove^ f collection)))))
  (defun %test-remove-if-not^ (find remove^ collection item)
    (let ((f (lambda (i) (not (eql i item)))))
      (%test collection find item (lambda () (funcall remove^ f collection))))))

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

(flet ((%test (count collection c old new result)
	 (ensure (< 0 c))
	 (ensure-same c (funcall count old collection))
	 (ensure-same 0 (funcall count old result))
	 (ensure-same c (funcall count new result))))
  (defun %test-substitute (substitute count collection old new)
    (let ((c (funcall count old collection))
	  (result (funcall substitute new old collection)))
      (%test count collection c old new result)))
  (defun %test-substitute-if (substitute count collection old new)
    (let ((c (funcall count old collection))
	  (result (funcall substitute new (lambda (i) (eql i old)) collection)))
      (%test count collection c old new result)))
  (defun %test-substitute-if-not (substitute count collection old new)
    (let ((c (funcall count old collection))
	  (result (funcall substitute new (lambda (i) (not (eql i old))) collection)))
      (%test count collection c old new result))))

(flet ((%test (count collection c old new result)
	 (ensure (< 0 c))
	 (ensure-same 0 (funcall count old collection))
	 (ensure-same c (funcall count new collection))
	 (ensure (equalp result collection))))
  (defun %test-substitute! (substitute count collection old new)
    (let ((c (funcall count old collection))
	  (result (funcall substitute new old collection)))
      (%test count collection c old new result)))
  (defun %test-substitute-if! (substitute count collection old new)
    (let ((c (funcall count old collection))
	  (result (funcall substitute new (lambda (i) (eql i old)) collection)))
      (%test count collection c old new result)))
  (defun %test-substitute-if-not! (substitute count collection old new)
    (let ((c (funcall count old collection))
	  (result (funcall substitute new (lambda (i) (not (eql i old))) collection)))
      (%test count collection c old new result))))

(defun %test-sort (sort collection expected cmp)
  (let ((orig (std:copy collection))
	(result (funcall sort collection cmp))
	(*lift-equality-test* #'equalp))
    (ensure-same expected result)
    (ensure-same orig collection)
    (ensure-null (equalp collection result))))

(defun %test-sort^ (sort collection expected cmp)
  (let ((orig (std:copy collection))
	(result (funcall sort collection cmp))
	(*lift-equality-test* #'equalp))
    (ensure-same expected result)
    (ensure-null (equalp orig collection))))

(defun %test-merge (merge collection output-spec one expected cmp)
  (let ((orig (std:copy collection))
	(result (funcall merge output-spec collection one cmp))
	(*lift-equality-test* #'equalp))
    (ensure-same expected result)
    (ensure-same orig collection)
    (ensure-null (equalp collection result))))

(defun %test-merge^ (merge collection output-spec one expected cmp)
  (let ((orig (std:copy collection))
	(result (funcall merge output-spec collection one cmp))
	(*lift-equality-test* #'equalp))
    (ensure-same expected result)
    (when (eql output-spec 'list)
      (ensure-null (equalp orig collection)))))

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
	      (symbol-macrolet ((type '',tp)
				(make-type '',mt))
		,@template))))
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

(defmacro define-specific-collection-suite-with-default-tests (name &key (key 1) (elem ''b) (change ''z) skip)
  (labels ((as-symbol (&rest args)
	     (intern (string-upcase (apply #'format nil args))))
	   (pkg-fun (f)
	     (intern (string-upcase f) name))
	   (as-name ()
	     (as-symbol "-~a-" name))
	   (as-pkg-fun (f)
	     `#',(pkg-fun f)))
    `(progn
       (deftestsuite ,(as-symbol "standard-collection-~a-test" name) (standard-collection-test)
	 ())
       ,@(unless (cl:find 'get skip)
		 `((addtest test-get
		     (%test-get ,(as-pkg-fun "get") ,(as-name) ,key ,elem))))
       ,@(unless (cl:find 'put! skip)
		 `((addtest test-put!
		     (%test-put! ,(as-pkg-fun "get") ,(as-pkg-fun "put!") ,(as-name) ,key ,elem ,change))))
       ,@(unless (cl:find 'copy skip)
		 `((addtest test-copy
		     (%test-base-copy ,(as-pkg-fun "copy") ,(as-name)))))
       ,@(unless (cl:find 'remove^ skip)
		 `((addtest test-remove^
		     (%test-remove^ ,(as-pkg-fun "find") ,(as-pkg-fun "remove^") ,(as-name) ,elem))))
       ,@(unless (cl:find 'remove skip)
		 `((addtest test-remove
		     (%test-remove ,(as-pkg-fun "find") ,(as-pkg-fun "remove") ,(as-name) ,elem)))))))

(deftestsuite standard-collection-test (standard-test)
  (-list- -array- -vector- -buffer- -string- -hash- -set-)
  (:setup
   (setf -list- (list 'a 'b 'c 'a))
   (setf -array- (array:make '(2 2) :initial-contents '((a b) (c a))))
   (setf -vector- (make-vector 'a 'b 'c 'a))
   (setf -buffer- (vector 'a 'b 'c 'a))
   (setf -string- (string:copy "abca"))
   (setf -hash- (hash:copy #{0 a, 1 b, 2 c, 3 a}))
   (setf -set- (set:copy #[a b c]))))

(add-collection-tests
 get
 %test-get (#'std:get) ((index 1) (expected 'b))
 (list (array (list 0 1))  vector buffer (string 1 #\b) hash))

(add-collection-tests
 put!
 %test-put! (#'std:get #'std:put!) ((index 1) (old 'b) (new 'z))
(list (array (list 0 1)) vector buffer (string 1 #\b #\z) hash))

(add-collection-tests
 length
 %test-length (#'std:length) ((expected 4))
 (list array vector buffer string hash (set 3)))

(add-collection-tests
 base-copy
 %test-base-copy (#'std:copy) ()
 (list array vector buffer string hash set))

(add-collection-tests
 position
 %test-position (#'std:position) ((item 'a) (expected 0))
 (list array vector buffer (string #\a) hash))

(add-collection-tests
 position-from-end
 %test-position (#'std:position) ((item 'a) (expected 3) (from-end t))
 (list array vector buffer (string #\a) hash))

(add-collection-tests
 position-if
 %test-position-if (#'std:position-if) ((item 'a) (expected 0))
 (list array vector buffer (string #\a) hash))

(add-collection-tests
 position-if-from-end
 %test-position-if (#'std:position-if) ((item 'a) (expected 3) (from-end t))
 (list array vector buffer (string #\a) hash))

(add-collection-tests
 position-if-not
 %test-position-if-not (#'std:position-if-not) ((item 'a) (expected 0))
 (list array vector buffer (string #\a) hash))

(add-collection-tests
 position-if-not-from-end
 %test-position-if-not (#'std:position-if-not) ((item 'a) (expected 3) (from-end t))
 (list array vector buffer (string #\a) hash))

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

; split, split-if, split-if-not

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
 remove
 %test-remove (#'std:find #'std:remove) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove-if
 %test-remove-if (#'std:find #'std:remove-if) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove-if-not
 %test-remove-if-not (#'std:find #'std:remove-if-not) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove^
 %test-remove^ (#'std:find #'std:remove^) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove-if^
 %test-remove-if^ (#'std:find #'std:remove-if^) ((item 'b))
 (list vector buffer (string #\b)))

(add-collection-tests
 remove-if-not^
 %test-remove-if-not^ (#'std:find #'std:remove-if-not^) ((item 'b))
 (list vector buffer (string #\b)))

;; NOTE: Hash makes no sense for remove-duplicates because keys are also relevant (how do you know which to keep?).  Array makes no sense because how would you truncate on subarray but not the other at that level
(add-collection-tests
 remove-duplicates
 %test-remove-duplicates (#'std:count #'std:remove-duplicates) ((item 'a) (starting 2))
 (list vector buffer (string #\a 2)))

(add-collection-tests
 remove-duplicates^
 %test-remove-duplicates^ (#'std:count #'std:remove-duplicates^) ((item 'a) (starting 2))
 (((list (list 'a 'b 'a 'a 'c 'a)) 'a 4) vector buffer (string #\a 2)))

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
 (list array vector buffer (string #\a #\z) hash))

(add-collection-tests
 substitute-if
 %test-substitute-if (#'std:substitute-if #'std:count) ((old 'a) (new 'z))
 (list array vector buffer (string #\a #\z) hash))

(add-collection-tests
 substitute-if-not
 %test-substitute-if-not (#'std:substitute-if-not #'std:count) ((old 'a) (new 'z))
 (list array vector buffer (string #\a #\z) hash))

(add-collection-tests
 substitute!
 %test-substitute! (#'std:substitute! #'std:count) ((old 'a) (new 'z))
 (list array vector buffer (string #\a #\z) hash))

(add-collection-tests
 substitute-if!
 %test-substitute-if! (#'std:substitute-if! #'std:count) ((old 'a) (new 'z))
 (list array vector buffer (string #\a #\z) hash))

(add-collection-tests
 substitute-if-not!
 %test-substitute-if-not! (#'std:substitute-if-not! #'std:count) ((old 'a) (new 'z))
 (list array vector buffer (string #\a #\z) hash))

(add-collection-tests-with-template
 sort
 %test-sort (#'std:sort) (expected (cmp #'<))
 ((list (type (make-type 3 1 4 2)) (make-type 1 2 3 4)))
 ((template list list)
  (template vector make-vector)
  (template buffer vector)
  ((string (std:copy "3142")) "1234" #'char<)))

(add-collection-tests-with-template
 sort^
 %test-sort^ (#'std:sort^) (expected (cmp #'<))
 ((list (type (make-type 3 1 4 2)) (make-type 1 2 3 4)))
 ((template list list)
  (template vector make-vector)
  (template buffer vector)
  ((string (std:copy "3142")) "1234" #'char<)))

;; TODO: Have to set up tests that verify sort is stable.  This probably can't be done for string since it only takes immediate types (chars)
;;
;; (add-collection-tests
;;  stable-sort
;;  %test-stable-sort (#'std:stable-sort) ()
;;  ())

;; (add-collection-tests
;;  stable-sort^
;;  %test-stable-sort^ (#'std:stable-sort^) ()
;;  ())

(add-collection-tests-with-template
 merge
 %test-merge (#'std:merge) (output-spec one expected (cmp #'<))
 ((list (type (make-type 1 3)) type (make-type 2 4) (make-type 1 2 3 4)))
 ((template list list)
  (template vector make-vector)
  ((buffer (vector 1 3)) 'vector (vector 2 4) (vector 1 2 3 4))
  ((string "13") 'string "24" "1234" #'char<)))

(add-collection-tests-with-template
 merge^
 %test-merge^ (#'std:merge^) (output-spec one expected (cmp #'<))
 ((list (type (make-type 1 3)) type (make-type 2 4) (make-type 1 2 3 4)))
 ((template list list)
  (template vector make-vector)
  ((buffer (vector 1 3)) 'vector (vector 2 4) (vector 1 2 3 4))
  ((string "13") 'string "24" "1234" #'char<)))

(define-specific-collection-suite-with-default-tests list)

(define-specific-collection-suite-with-default-tests array :key (list 0 1) :skip (get put! remove remove^))

(addtest test-get
  (ensure-same 'b (array:get -array- 0 1)))

(define-specific-collection-suite-with-default-tests vector)

(define-specific-collection-suite-with-default-tests buffer)

(define-specific-collection-suite-with-default-tests string :elem #\b :change #\z)

(define-specific-collection-suite-with-default-tests hash :skip (remove remove^))

(define-specific-collection-suite-with-default-tests set :skip (get put! remove remove^))