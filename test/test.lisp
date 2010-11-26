(in-package #:standard-test)

(defun run-all-tests ()
  (run-tests :suite 'standard-test))

(deftestsuite standard-test ()
  (-list-)
  (:setup
   (setf -list- (list 'a 'b 'c 'a))))
