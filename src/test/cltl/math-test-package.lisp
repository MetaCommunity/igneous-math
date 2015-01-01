;; math-test-package.lisp - package definition for igneous-math tests

(in-package #:cl-user)

(defpackage #:info.metacommunity.cltl.math.test
  (:nicknames #:mcicl.math.test)
  (:use
   #:info.metacommunity.cltl.test
   #:info.metacommunity.cltl.utils
   #:cl
   ))
