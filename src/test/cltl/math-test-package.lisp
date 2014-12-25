;; math-test-package.lisp - package definition for igneous-math tests

(in-package #:cl-user)

(defpackage #:info.metacommunity.cltl.math.test
  (:nicknames #:math.test)
  (:use
   #:info.metacommunity.cltl.test
   #:info.metacommunity.cltl.utils
   #:cl
   ))
