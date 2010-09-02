(in-package #:cl-user)

(defpackage #:standard.collection
  (:nicknames #:std.collection #:collection)
  (:use #:cl)
;  (:import-from #:std.functional #:uncurry) ;TODO: do we really need these functions?  Only used by the hash print/read code
  (:shadow #:get #:set #:remove #:member #:union #:intersection #:complement #:subsetp)
  (:export
   #:get
   #:set
   #:remove
   #:remove!
   #:copy
   #:lref
   #:copy-list
   #:href
   #:member
   #:union
   #:union!
   #:intersection
   #:intersection!
   #:complement
   #:complement!
   #:subsetp))