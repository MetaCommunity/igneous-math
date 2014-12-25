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
  #+CCL ;; FIXME MOP-PORT
  (:shadowing-import-from
   #:ccl
   #:compute-effective-method
   ;; #:make-method-lambda ;; not defined in CCL (?)
   ;; #:compute-applicable-methods
   )
  #+SBCL ;; FIXME: MOP-PORT
  (:shadowing-import-from
   #:SB-PCL
   #:method-combination-type-name
   )
  #+(or SBCL CMU CCL) ;; FIXME: MOP-PORT
  (:shadowing-import-from
   #+SBCL #:SB-MOP
   #+CMU #:PCL
   #+CCL #:CCL
   #+ALLEGRO #:MOP
   #:validate-superclass
   )

  (:use #:info.metacommunity.cltl.utils
        #:bordeaux-threads
        #:c2mop
        #:cl)

  (:export
   ;; utils
   ;;FIXME: Consider moving these into the utils system
   #:entity-condition
   #:entity-condition-name
   #:entity-not-found

   #:redefinition-condition
   #:redefinition-condition-previous-object
   #:redefinition-condition-new-object

   #:container-condition
   #:container-condition-container

   ;; #:vsubsetp ;; move this into the utils system
   )

  (:export
   #:measurement-domain
   #:measurement-domain-base-measure
   #:measurement-domain-symbol
   ;;  #:measurement-domain-cf-lock
   ;;  #:measurement-domain-conversion-factors
   
   #:domain-of
   #:measurement-domain-not-found
   #:measurement-domain-redefinition
   #:find-measurement-domain
   #:register-measurement-domain
   #:enumerate-measurement-domains
   
   #:integer-shift-digits
   #:float-shift-digits

   #:measurement-symbol
   #:measurement-base-factor
   #:measurement-base-factor-exponent
   #:measurement-class
   #:measurement-class-designator
   #:measurement-class-not-found
   #:measurement-class-redefinition
   ;; #:linear-measurement-class
   #:register-measurement-class
   #:find-measurement-class
   #:conversion-factor
   #:conversion-factor-source-unit
   #:conversion-factor-magnitude
   #:conversion-factor-magnitude
   #:conversion-factor-exponent
   #:conversion-factor-destination-unit
   #:make-conversion-factor
   #:find-conversion-factor
   #:register-conversion-factor
   #:measurement-magnitude
   #:measurement-degree
   #:measurement   
   #:measurement-base-measure
   #:measurement-symbol
   #:measurement-symbol
   #:base-measurement-class
   #:derived-measurement-class
   #:length #:meter #:foot
   #:mass #:kilogram #:gram
   #:time #:second
   #:electrical-current #:ampere
   #:temperature #:kelvin
   #:amount-substance #:mole
   #:luminous-intensity #:candela

   #:rescale
   #:nrescale
   #:prefix-degree
   #:prefix-symbol
   #:find-nearest-degree
   #:prefix
   #:prefix-not-found
   #:prefix-degree-not-found
   #:find-prefix
   #:find-prefix=
   #:register-prefix
   #:prefix-of
   #:shift-magnitude
   #:scale-si
   
   #:conversion-domains-mismatch
   #:conversion-source-domains
   #:conversion-destination-domains
   ;; #:verify-conversion-domain ;; internal util
   #:convert-measurement
   ;; #:base-convert-measurement ;; ambiguous function
   ;; #:base-magnitude ;; ambiguous function
   ;; #:scalar-magnitude ;; ambiguous function
   )
  
  )


(in-package #:math)
;; FIXME: "Do this somewhere else"
#-(or SBCL CMU)
(declaim (declaration values))
