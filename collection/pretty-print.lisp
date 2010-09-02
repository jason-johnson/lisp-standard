(in-package #:std.collection)

;; Setup all the pretty print statements in the correct order (i.e. subtypes before super types)

(set-pprint-dispatch 'set #'std.collection.set::write-set)   ; NOTE: ensure this is defined before the hash one or it will never be used (if the implementation is a hash) (these need to all be collected in one file)
(set-pprint-dispatch 'hash-table #'std.collection.hash::write-hash)
