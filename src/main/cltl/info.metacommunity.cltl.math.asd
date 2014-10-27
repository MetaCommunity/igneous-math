;; info.metacommunity.cltl.math.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:math-system
    (:use #:asdf #:cl)))

(in-package #:math-system)



#|

Effective version history:

 1.0: Initial prototype

 1.1: Decimal scaling introduced

 1.2: DEFOP defined with MOP extension

 1.3: Definition of unit conversion formulas

|#

(defsystem #:info.metacommunity.cltl.math
  :description 
  "Implementation of a Mathematical System in Common Lisp"
  :version "1.3"
  ;; :homepage "https://github.com/MetaCommunity/igneous-math"
  ;; :license "https://github.com/MetaCommunity/igneous-math/blob/master/LICENSE"
  :depends-on 
  (#:info.metacommunity.cltl.utils
   #:closer-mop
   #:bordeaux-threads)
  :serial t
  :components 
  ((:file "math-package")
   (:file "defclass-star") ;; FIXME: move into .utils system
   (:file "monotonic-genf") ;; FIXME: move into .utils system
   (:file "math-ov")
   (:file "measurement")
   (:file "decimal-scale")
   (:file "prefix")
   (:file "geometry")
   ))
