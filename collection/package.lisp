(in-package #:cl-user)

(defpackage #:standard.collection
  (:nicknames #:std.collection #:collection)
  (:use #:cl #:std.base)
;  (:import-from #:std.functional #:uncurry) ;TODO: do we really need these functions?  Only used by the hash print/read code
  (:shadow #:get #:remove #:map)
  (:export
   #:get
   #:put!
   #:copy
   #:remove
   #:remove!
   #:map-to))