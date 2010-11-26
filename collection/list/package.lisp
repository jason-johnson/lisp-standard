(in-package #:cl-user)

(defpackage #:standard.collection.list
  (:nicknames #:std.collection.list #:collection.list #:list)
  (:use #:cl #:impl-common)
  (:import-from #:std.base #:defun-alias #:compose)
  (:shadow #:get #:put! #:copy #:length #:do #:map #:concatenate #:sort #:stable-sort #:merge)
  (:export
   #:make
   #:get
   #:put!
   #:copy
   #:copy^
   #:deep-copy
   #:do
   #:length
   #:concatenate
   #:push!
   #:pop!
   #:map
   #:map-flatten
   #:maplist
   #:maplist-flatten
   #:foreach
   #:foreach-list
   #:last
   #:butlast
   #:butlast^
   #:ldiff				; TODO: Needed?
   #:tailp
   #:reverse-append
   #:reverse-append^
   #:reduce
   #:find
   #:find-if
   #:find-if-not
   #:remove
   #:remove^
   #:remove-if
   #:remove-if^
   #:remove-if-not
   #:remove-if-not^
   #:remove-duplicates
   #:remove-duplicates^
   #:reverse
   #:reverse^
   #:substitute
   #:substitute!
   #:substitute-if
   #:substitute-if!
   #:substitute-if-not
   #:substitute-if-not!
   #:split^
   #:split-if^
   #:split-if-not^
   #:split
   #:split-if
   #:split-if-not
   #:append
   #:append!
   #:sort
   #:sort^
   #:stable-sort
   #:stable-sort^
   #:merge
   #:merge^))
