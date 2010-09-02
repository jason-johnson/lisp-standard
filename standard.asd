;;; -*- lisp -*- system definition

(in-package #:asdf)

(defsystem #:standard
  :description "Standard library for lisp - alternative to cl"
  :depends-on (#:standard.core #:standard.library))