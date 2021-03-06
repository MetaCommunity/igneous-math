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
  :version "1.3.2"
   :homepage "https://github.com/MetaCommunity/igneous-math"
   :license "https://github.com/MetaCommunity/igneous-math/blob/master/LICENSE"
  :depends-on 
  (#:info.metacommunity.cltl.utils
   #:closer-mop
   #:bordeaux-threads
   #:info.metacommunity.cltl.utils.mop
   )
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

   (:module "measure"
            :depends-on ("math-package"
                         "defclass-star"
                         "math-system-utils")
            :components
            ((:file "domain")
             (:file "decimal-scale")
             (:file "measurement-base"
                    ;; :subsystem measure
                    :depends-on ("domain"
                                 "decimal-scale"
                                 ))

             (:module "derived"
                      :depends-on ("measurement-base")
                      :components
                      ((:file "linear-measure")
                       (:file "geom-measure")
                       (:file "compound-measure"
                              :depends-on 
                              ("linear-measure" 
                               "geom-measure"
                               ))
                       (:file "dimensionless")
                       (:file "derived-length")))
             
             (:file "prefix"
                    ;; :subsystem measure
                    ;; nb. The prefix system is essentially orthogonal to MAKE-MEASUREMENT
                    :depends-on ("measurement-base"
                                 ))
             (:file "mconv"
                    :depends-on ("prefix"
                                 "measurement-base"
                                 "domain"))

             ))

   (:file "measure/measurement-ov" 
          ;; measurements + math
          :depends-on ("measure"
                       "math-ov"))
   
   (:file "geometry"
	  :depends-on ("measure"))

   (:file "linear"
          :depends-on ("math-package"))

   ))
