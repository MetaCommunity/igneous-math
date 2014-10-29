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
  "A Mathematical Object System in Common Lisp"
  :version "1.3"
   :homepage "https://github.com/MetaCommunity/igneous-math"
   :license "https://github.com/MetaCommunity/igneous-math/blob/master/LICENSE"
  :depends-on 
  (#:info.metacommunity.cltl.utils
   #:closer-mop
   #:bordeaux-threads)
  ;; :serial t
  :components 
  ((:file "math-package")
   (:file "math-system-utils"
	  :depends-on ("math-package"))

   (:file "defclass-star"  ;; FIXME: move into .utils system
	  :depends-on ("math-package"))
   (:file "monotonic-genf"  ;; FIXME: move into .utils system
	  :depends-on ("math-package"))

   (:file "math-ov"
	  ;; NB: This file is essentially orthogonal to the
	  ;; measurements subsystem
	  :depends-on ("monotonic-genf"
		       "math-package"))

   (:file "domain"
	  ;; :subsystem measure
	  :depends-on ("math-system-utils"
		       "defclass-star"))
   (:file "measurement"
	  ;; :subsystem measure
	  :depends-on ("domain"
		       "decimal-scale"
		       "math-system-utils"
		       "defclass-star"))

   (:file "decimal-scale"
	  ;; :subsystem measure
	  :depends-on ("math-package"))

   (:file "prefix"
	  ;; :subsystem measure
	  ;; nb. The prefix system is essentially orthogonal to MAKE-MEASUREMENT
	  :depends-on ("measurement"
		       "math-system-utils"
		       "defclass-star"))

   (:file "geometry"
	  :depends-on ("measurement"))

   ))
