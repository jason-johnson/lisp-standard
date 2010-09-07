(in-package #:standard-test.collection)

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
   (put-test (make put! list f verify)
	     (let* ((size (list:length list))
		    (it (funcall make size)))
	       (dotimes (i size)
		 (funcall put! it i (list:get list i)))
	       (let ((result (funcall f it)))
		 (funcall verify it result))))))

(addtest test-get
  (flet ((ensure-it (collection index expected)
	     (ensure-same expected (collection:get collection index))))
    (ensure-it -list- 1 'b)
    (ensure-it -vector- 1 'b)
    (ensure-it -buffer- 1 'b)
    (ensure-it -string- 1 #\b)
    (ensure-it -hash- 2 'b)))

(addtest test-put!
  (flet ((ensure-it (collection index old new)
	     (ensure-same old (collection:get collection index))
	     (collection:put! collection index new)
	     (ensure-same new (collection:get collection index))))
      (ensure-it -list- 1 'b 'z)
      (ensure-it -vector- 1 'b 'z)
      (ensure-it -buffer- 1 'b 'z)
      (ensure-it -string- 1 #\b #\z)
      (ensure-it -hash- 2 'b 'z)))

(addtest test-copy
  (flet ((ensure-it (collection)
	   (let ((result (collection:copy collection))
		 (lift:*lift-equality-test* 'equalp))
	     (ensure-same result collection)
	     (ensure-null (eq result collection)))))
    (ensure-it -list-)
    (ensure-it -array-)
    (ensure-it -vector-)
    (ensure-it -buffer-)
    (ensure-it -string-)
    (ensure-it -hash-)
    (ensure-it -set-)))

(deftestsuite standard-collection-buffer-test (standard-collection-test)
  ()
  (:function
   (put-test-local (f verify)
     (put-test
      #'buffer:make
      #'buffer:put!
      -list-
      f
      verify))))

(addtest test-remove^
  (put-test-local
   (lambda (it)
     (buffer:remove^ 'b it))
   (lambda (it result)
     (ensure-null (find 'b result))
     (ensure-null (find 'b it)))))

(addtest test-remove
  (put-test-local
   (lambda (it)
     (buffer:remove 'b it))
   (lambda (it result)
     (ensure-null (find 'b result))
     (ensure-same 'b (find 'b it)))))
