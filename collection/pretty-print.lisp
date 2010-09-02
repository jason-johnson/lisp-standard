(in-package #:std.collection)

;; Setup all the pretty print statements in the correct order (i.e. subtypes before super types)

(set-pprint-dispatch 'std.collection.set:set #'std.collection.set::write-set)
(set-pprint-dispatch 'hash-table #'std.collection.hash::write-hash)
