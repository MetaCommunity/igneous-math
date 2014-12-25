;; info.metacommunity.cltl.math.test.asd - math system tests  -*- lisp -*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:math-system
    (:use #:asdf #:cl)))

(in-package #:math-system)

(defsystem #:info.metacommunity.cltl.math.test
  :depends-on 
  (#:info.metacommunity.cltl.math
   #:info.metacommunity.cltl.test
   #:info.metacommunity.cltl.utils
   )
  :components
  ((:file "math-test-package")
   (:file "measure-test"
          :depends-on ("math-test-package"))
   ))
