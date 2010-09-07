(in-package #:net-telnet)

; Who needs mocks?
(defun option-send (telnet command option)
  (let ((cmd (ecase command
	       (#.+WILL+ 'WILL)
	       (#.+WONT+ 'WONT)
	       (#.+DO+ 'DO)
	       (#.+DONT+ 'DONT))))
    (format t "SENT COMMAND ~a for option ~a" cmd (char-code option))))

(in-package #:net-telnet-test)

(deftestsuite net-telnet-test ()  
 ((-echo- net-telnet::+ECHO+)
  -telnet-)
 (:setup  
  (setf -telnet- (net-telnet:telnet "localhost" 23 10 nil)))
 (:function
  (check-states (option local remote)
		(flet ((get-state (location)
			 (net-telnet::os-state (svref (gethash option (net-telnet::options -telnet-)) location))))
		  (ensure-same (get-state net-telnet::+LOCAL+) local)
		  (ensure-same (get-state net-telnet::+REMOTE+) remote))))
 (:function
  (check-echo-states (local remote) (check-states -echo- local remote))))

(deftestsuite net-telnet-test-option-negotiation (net-telnet-test)
  ())

(addtest check-options
  (check-echo-states 'net-telnet::NO 'net-telnet::ACCEPT)
  (check-states net-telnet::+SGA+ 'net-telnet::NO 'net-telnet::ACCEPT))

(addtest check-accept
  (net-telnet::option-accept -telnet- -echo- net-telnet::+LOCAL+)
  (check-echo-states 'net-telnet::ACCEPT 'net-telnet::ACCEPT))

;(addtest check-enable
;  (net-telnet