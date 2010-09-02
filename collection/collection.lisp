(in-package #:std.collection)

;; TODO: Actually this is going to have to somehow be defined before the package is actually defined (like is done with standard itself) because collection actually wants to expose all of list, hash, array, etc.
;; TODO: On the other hand, that might not be the case because it may be that literally everything that comes from these packages is re-aliased as something else for collection
;; TODO: The third possibility (and probably the one I'll go with) is to simply specify every symbol, by hand, in the collection package.  (and how do these symbols make it into collection then?  Must be with alias I think)

;; TODO: Actually, what I'm doing now is fine.  We just don't export symbols that would be exactly the same except with a - instead of :  (e.g. list-copy vs list:copy).  The exception is the make functions, which will be aliased.
;; TODO: And that's why what I'm doing now works

(defgeneric get (container key)
  (:documentation "Generic access method"))

(defgeneric put! (container key value)
  (:documentation "Generic set method"))
