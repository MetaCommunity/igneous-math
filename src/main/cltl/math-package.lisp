;; math-package.lisp

(in-package #:cl-user)

(defpackage #:info.metacommunity.cltl.math
  (:nicknames #:math)
  (:shadowing-import-from 
   ;; prefer the implementation's own forms to those defined in c2mop
   ;; towards debugging, etc.
   #:cl
   #:defgeneric
   #:defmethod
   #:standard-generic-function
   #:standard-method
   #:standard-class
   )
  #+CCL ;; FIXME
  (:shadowing-import-from
   #:ccl
   #:compute-effective-method
   ;; #:make-method-lambda ;; not defined in CCL (?)
   ;; #:compute-applicable-methods
   )
  #+(or SBCL CMU CCL)
  (:shadowing-import-from
   #+SBCL #:SB-MOP
   #+CMU #:PCL
   #+CCL #:CCL
   #:validate-superclass
   )

  (:use #:info.metacommunity.cltl.utils
        #:bordeaux-threads
        #:c2mop
        #:cl)


  (:export
   ;; measurements module
   #:length #:meter #:foot
   #:mass #:kilogram
   #:time #:second
   #:electrical-current #:ampere
   #:temperature #:kelvin
   #:amount-substance #:mole
   #:luminous-intensity #:candela
   #:convert-measurement
   ))


(in-package #:math)
;; FIXME: "Do this somewhere else"
#-(or SBCL CMU)
(declaim (declaration values))
