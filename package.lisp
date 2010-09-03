(in-package #:cl-user)

;; TODO: I'd like to be able to use variables here for the compliment packages and the exclusion packages but I'm not sure how to make them visable here without using another file (and what package would we store the name in?)
(std.base:def-metapackage-compliment standard.core (cl) (std.base std.collection) () (std.core))

(std.base:def-metapackage standard (std.base std.collection std.core) (std))

(defpackage #:standard-user
  (:nicknames #:std-user)
  (:use #:std))

;; TODO: At some point base will provide its own version of defpackage that gives more options (e.g. qualify to rename a package, which might make a meta package of the qualified name, or might try a symbol macro) and imports
;; TODO: std.core along with whatever else the use specifies.  This allows the user to do things like:

#|
(in-package #:std-user)

(defpackage #:example
  (:use #:std.collection))

|#

;; TODO: And it will make sure std.core is included as well so that the user can define functions etc.... is this actually a good idea?  cl doesn't work like this.  I think it's probably a bad idea.  Better to just document
;; TODO: That users will want to use std.core for the base system if they are only including certain packages.  They can, of course, just include standard to get everything