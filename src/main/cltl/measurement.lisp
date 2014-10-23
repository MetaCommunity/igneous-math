
(in-package #:math)

;; TO DO:
;; 1) Define an ASDF system for this source file
;;
;; X) Define a reader macro syntax for measurement units
;;    also cf. Jakub Higersberger's unit-formula system
;;
;; Y) Extend MEASUREMENT-CLASS with a new class,
;;    DERIVED-MEASUREMENT-CLASS, such that would publish an accessor
;;    for calculating a formula for measurement unit converstions
;;
;; 2) Implement YouTrack and TeamCity onto AWS (cf. Nr 4)
;;
;; 3) Move DEFCLASS* into the mci-cltl-utils source tree
;;
;; 4) Define, within YouTrack and TeamCity, such TO DO items as are
;;    denoted within the commentary in the DEFCLASS* macro definition 
;;
;; 5) Define a convenient syntax for extension of this measurement
;;    protocol in definition of custom measurement units
;;
;; 6) Continue with definition of the geometry component of this
;;    system, toward DEFCLASS VECTOR etc.
;;
;; 7) "Back track" to the TO DO items defined then in YouTrack/TeamCity
;;
;; 8) At some point, refine the comments in this file into a form of
;;    normative documentation for this program system
;; 
;; 9) Define a desktop interface for this system - carefully avoiding
;;    any manner of a competitive spirit towards Wolfram Mathematica
;;    and the respective MathML implementations available within the
;;    contemporary computing domain - albeit all within proprietary/
;;    closed source software systems, "those" -- also maxing reference
;;    onto the ACL2 "theorem prover" system  and Maxima. Although this
;;    system is being defined morso for a purpose of supporting
;;    applicationso of analytic geometry, as primarily with regards
;;    to electical engineering, however there must be some references
;;    made onto theoretical mathematics, throughout this system.


;; sidebar: measurement definitions in hytime
;;
;; cf. 
;; http://www.cs.utexas.edu/users/novak/units95.html
;;
;; http://www.bipm.org/en/publications/si-brochure/metre.html
;;  -> http://www.bipm.org/en/publications/si-brochure/download.html
;;
;; http://www.is-thought.co.uk/schedule.htm
;; http://crism.maden.org/consulting/pub/hytime/meas.html
;; http://www.hytime.org/materials/hi2mdhyt.sgm
;; ^ cf %hygrand, "


;; see also:
;; http://physics.nist.gov/cuu/units/
;; http://physics.nist.gov/pubs/sp811/appenb.html 
;; ^ esp. for conversions regarding foot, mile, yard , ...


;; referencing http://www.bipm.org/en/publications/si-brochure/
;; and http://physics.nist.gov/pubs/sp811/contents.html
    

(defmacro defclass* (name-form (&rest superclasses) 
                             (&rest slot-definitions)
                     &rest initargs)
  ;; FIXME: This macro does not, of itself, provide a complete
  ;; "DEFCLASS* SOMEWHAT LIKE DEFSTRUCT"  implementation. Features
  ;; presently lacking of this implementation, in that regards
  ;; may include:
  ;; 
  ;; * Many of the features avaialble for structure classes defined
  ;;   via DEFSTRUCT are altogether lacking from this protocol, such
  ;;   as: 
  ;;    * Naming and definition of a <type-p> predicate function
  ;;    * Naming and definition of a <copier> function
  ;;    * Specification of an object printer function
  ;;    * Naming and lambda list syntax for constructor functions
  ;;    * Nothing such as DEFSTRUCT's linear :INHERIT
  ;;    * Nothing such as a list type or vector type syntax for instances
  ;;    * Other qualities, as denoted in the following
  ;;
  ;; * In that this DEFCLASS* macro implements a syntax "Somewhat like
  ;;   defstruct", the following features are noted:
  ;;
  ;;   * NAME-FORM may be a symbol or a list. If a symbol, then
  ;;     NAME-FORM denotes the name of the class. If a list, then
  ;;     NAME-FORM denotes -- as its first element -- the name of the
  ;;     class, with a syntax similar to DEFTTRUCT <name-and-options>.
  ;;    
  ;;     DEFCLASS* emulates DEFSTRUCT' :CONC-NAME option
  ;;
  ;;   * For all direct slots of the class, a set of reader and writer
  ;;     methods will be defined, as named according to CONC-NAME
  ;;     interpreted in a manner similar to as with DEFSTRUCT (FIXME:
  ;;     However, in its present revisin, DEFCLASS* rather interprets
  ;;     a NULL CONC-NAME as a "flag" that no reader or writer methods
  ;;     should be defined). Unless the slot-definition is denoted as
  ;;     :READ-ONLY, a writer method will be defined for the slot
  ;;     definition, named as per <that naming convention>. In <all
  ;;     instances>, a reader method will be defined for the slot
  ;;     instances>definition (NOTE: Unless CONC-NAME is NULL

  ; FIXME: Redefine DEFCLASS* to interpret CONC-NAME in a manner more
  ; consissent onto DEFSTRUCT; provide an additional slot definition
  ; "flag" value for denoting if a slot defintion is not to have any
  ; reader or writer methods defined for it; revise this
  ; documentation, subsequent to that changeset. (Firstly, move
  ; DEFCLASS* into the mci-cltl-utils source tree). Lastly, describe
  ; the complete syntax for slot definition specifiers, as implemented
  ; of this DEFCLASS* macro -- and make a more direct reference to the
  ; X3J13 discussions surrounding the definition of the syntax of each
  ; of DEFSTRUCT and DEFCLASS in CLtL2 -- all of this, to proceed
  ; after implementation of Jetbrains' YouTrack and TeamCity
  ; components into a Glasfish server in an AWS instance.

  ;;
  ;; * When a class' slot is redefined from "not read only" to "read
  ;;   only" then the SLOT-DEFINITION-WRITER methods defined for slots
  ;;   in the class as side-effects of its definition as "not read
  ;;   only" are not undefined. Those "then invalid" writer methods
  ;;   should be made undefined, however, as consequent with the
  ;;   change in slot definition qualities
  ;;
  ;; * Concerning the :READ-ONLY value for slot specifiers for this
  ;;   macro, presently that value may seem to represent something of
  ;;   a misnomer. <Presently> :
  ;;
  ;;     * The READ-ONLY value is not stored with the slot
  ;;       definition - whether its direct slot definition of
  ;;       effective slot defintion
  ;;
  ;;     * The READ-ONLY value will not be inherited by subclasses
  ;;
  ;;     * The READ-ONLY value does not actually prevent SETF access
  ;;       to slots, as by way of (SETF SLOT-VALUE)
  ;;
  ;;   Those shortcomings may be addressed, subsequently, with a
  ;;   definition of a READ-ONLY-INSTANCE-SLOT-DEFINITION
  ;;   protocol. However, as one's experiences in developing
  ;;   extensions onto CLOS might seem to prove: Once the proverbial
  ;;   Pandora's box of slot definition extension is opened, then --
  ;;   in less figurative terms -- it may be difficult to develop any
  ;;   effectively "layered" extensions onto STANDARD-SLOT-DEFINITION,
  ;;   as well as the direct and effective slot definition subclasses
  ;;   of STANDARD-SLOT-DEFINITION. Certainly, that is not to
  ;;   criticise the design of MOP, whereas -- presently -- one
  ;;   considers that it may be possible to develop an extensional
  ;;   architecture for facilitating a definition of "layered",
  ;;   domain-specific slot definition extensions
  ;;
  ;;   As well as that such an architecture may be applied in
  ;;   developing this READ-ONLY-INSTANCE-SLOT-DEFINITION proposal,
  ;;   but furthtermore: Such an architecture may be applied for
  ;;   developing a MODALLY-LOCKED-SLOT-DEFINITION protocol namely
  ;;   using read-write locking onto slot values, as may be
  ;;   facilitative of thread safety in Common Lisp programs.
  ;;
  ;;   Thirdly, such an architecture may be applied for developing an
  ;;   L10N-SLOT-DEFINITION proposal, namely to facilitate
  ;;   internationalization of locale-specific values - especially,
  ;;   language-specific strings -- within Common Lisp applications.
  ;;   See also: The #I18N notes peppered throughout the codebases
  ;;   provided of the MetaCommunity.info project
  ;;
  ;;  Thus, effectively five "Architectural FIXME" items are defined here:
  ;;
  ;;  * DEFCLASS*-MORE-SOMEWHAT-LIKE-DEFSTRUCT
  ;;  * LAYRED-SLOT-DEFINITON-ARCHITECTURE
  ;;  * READ-ONLY-SLOT-DEFINITION
  ;;  * MODALLY-LOCKED-SLOT-DEFINITION
  ;;  * L10N-SLOT-DEFINITION liketly to be followed with
  ;;    I18N-STRING-SLOT-DEFINITION and a broader I18N protocol
  ;;    integrating with existing internationalization frameworks, e.g
  ;;    "PO files"
  (destructuring-bind (name &key (conc-name nil cnp))
      (cond
        ((symbolp name-form) (list name-form))
        (t name-form))

    (unless (or cnp conc-name)
      (setq conc-name (make-symbol (format nil "~A-" name))))

    (labels ((acc (slot read-only-p)
               (when conc-name
                 (list (if read-only-p :reader :accessor)
                       (intern-formatted "~A~A" conc-name slot))))
             (initarg (slot)
               (list :initarg (intern (symbol-name slot) :keyword)))

             (initform (form infp)
               (when infp
                 (list :initform form))))
             
      `(defclass ,name (,@superclasses) 
         ,(mapcar (lambda (spec)
                    (destructuring-bind (name &optional (type t)
                                              &key read-only
                                              (initform nil infp))
                        spec
                      `(,name ,@(acc name read-only)
                          ,@(initarg name)
                          :type ,type
                          ,@(initform initform infp))))
                  slot-definitions)
         ,@initargs))))

#+NIL
(macroexpand-1 (quote
(defclass* (frob :conc-name #:a-frob-) ()
  ((b)
   (c real :read-only t)))
))

(defgeneric measurement-quantity-name (instance))
(defgeneric measurement-print-label (instance))
(defgeneric measurement-print-name (instance))
(defgeneric measurement-symbol (instance))

(defclass* (measurement-domain
            :conc-name #:measurement-)
    ()
  ((quantity-name simple-string :read-only t)
   (print-label simple-string :read-only t)
   (print-name simple-string :read-only t)
   (symbol symbol :read-only t)))


(defclass measurement-class (measurement-domain standard-class)
  ())


(deftype measurement-class-designator ()
  '(or symbol measurement-class))


(define-condition entity-not-found (error)
  ((name
    :initarg :name
    :reader entity-not-found-name)))

(define-condition class-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "No measurement class registered for name ~S"
             (entity-not-found-name c)))))

   

#+SBCL ;; FIXME: also port to other impmls. using PCL
(defmethod sb-mop:validate-superclass ((class measurement-class)
                                       (superclass standard-class))
  (values t))


(let ((%classes% (make-array 7 :fill-pointer 0))
      (%classes-lock% (make-lock "%CLASSES%")))
  (defun register-measurement-class (c)
    (declare (type measurement-class c))
    (with-lock-held (%classes-lock%)
      (let* ((s (measurement-symbol c))
             (n (position s %classes%
                          :test #'eq
                          :key #'measurement-symbol)))
        (cond 
          ((and n (not (eq (aref %classes% n) c)))
           (simple-style-warning 
            "Redfining measurement class for ~S" s)
           (setf (aref %classes% n) c))
          (t (vector-push-extend c %classes%)))
        (values c))))

  (defun find-measurement-class (s)
    (declare (type symbol s)
             (values measurement-class))
    (with-lock-held (%classes-lock%)
      (or (find s %classes%
               :test #'eq
               :key #'measurement-symbol)
          (error 'class-not-found :name s))))
  )
  



(defgeneric measurement-magnitude (instance))

;; FIXME: rename to measurement-scale
(defgeneric measurement-degree (instance))


(defclass* measurement ()
  ;; magnitude : a real value that, when multplied by 10^degree,
  ;; then serves as an effective element for calculating the
  ;; `scalar-magnitude' for the measurement
  ((magnitude real :initform 0)
   ;; degree : an exponent of 10, denoting the degree of the decimal
   ;; prefix factor for the measurement; serves as like a scale
   ;; value. see also: PREFIX class and protocol
   ;;
   ;; TO DO: For measures of information content (byte, etc) define an 
   ;; additional prefix system - onto powers of 8, rather than of 10.
   ;; The DEGREE slot for MEASUREMENT may be interpreted in simply an
   ;; alternate regards, then -- as in (expt 8 DEGREE) rather than 
   ;; (expt 10 DEGREE) -- respectively, an effective FACTOR-BASE of 8
   ;; rather than 10
   (degree fixnum :initform 0)
   ;; FIXME: For purpose of memoization, consider developing a
   ;; MEMOIZED-SLOT protocol such that a slot
   ;; MEMOIZED-FACTORED-MAGNITUDE would store an effective 
   ;; cached value of (* magnitude (expt factor-base degree)) 
   ;; Referencing Garnet KR, the same slot may be defined effetively
   ;; with a formula onto other slots in the class and other formulas
   ;; iin the Lisp environment
   ))


;; FIXME: Move this DEFCONSTANT into prefix handling code, and consider
;; refactoring this system for defining an accessor FACTOR-BASE onto a
;; new class PREFIX-CLASS - such that would allow for defining
;; prefixes for an octal factor base, as for measurements of data
;; quantities within information systems
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant %factor-base% 10)
  )

(defun base-magnitude (m)  
  "Calculate the scalar magnitude of the measurement M for the base
measurement unit of M"
  ;;
  ;; NB. This function implements a numeric conversion on
  ;; a basis of of prefix magnitude. Conversions per ratios of derived
  ;; units must be implemneted seperately.
  (declare (type measurement m)
           (values real))
  (let ((deg (measurement-degree m)))
    (cond 
      ((zerop deg) (measurement-magnitude m))
      (t (* (measurement-magnitude m)
            (expt #.%factor-base% deg))))))

;; (base-magnitude (make-measurement 1 :m 3))
;; => 1000

;; (base-magnitude (make-measurement 1 :m -3))
;; => 1/1000

(defgeneric scalar-magnitude (scalar)
  (:method ((scalar measurement))
    "Return the magnitude of the SCALAR measurement onto the base unitx
for the measurement"
    (base-magnitude scalar)))

;; (scalar-magnitude (make-measurement 1 :m -3))
;; => 1/1000


;;; % MEASUREMENT

(defmethod measurement-quantity-name ((instance measurement))
  (measurement-quantity-name (class-of instance)))

(defmethod measurement-print-label ((instance measurement))
  (measurement-print-label (class-of instance)))

(defmethod measurement-print-name ((instance measurement))
  (measurement-print-name (class-of instance)))

(defmethod measurement-symbol ((instance measurement))
  (measurement-symbol (class-of instance)))


;;; define base unit classes
(labels ((do-def (c quantity print-label print-name name)
           (let ((c (c2mop:ensure-class 
                     c
                     :direct-superclasses (list (find-class 'measurement))
                     :symbol name
                     :print-name print-name
                     :print-label print-label
                     :quantity-name quantity
                     :metaclass (find-class 'measurement-class)
                     #+SBCL :definition-source 
                     #+SBCL (sb-c:source-location) 
                     )))
             (register-measurement-class c))))
  (mapcar (lambda (spec)
            (destructuring-bind 
                  (c quantity print-label print-name name) spec
              (do-def c quantity print-label print-name name)))
          '((meter "length" "metre" "m" :m)
            (kilogram "mass" "kilogram" "kg" :kg)
            (second "time, duration" "second" "s" :s)
            (ampere "electric current" "ampere" "A" :a)
            (kelvin "thermodyamic temperature" "kelvin" "K" :k)
            (mole "amount of substance" "mole" "mol" :mol)
            (candela "luminous intensity" "candela" "cd" :cd)))
  )

;; (find-measurement-class :m)
;; (find-measurement-class :kg)
;; (find-measurement-class :s)
;; (find-measurement-class :a)
;; (find-measurement-class :k)
;; (find-measurement-class :mol)
;; (find-measurement-class :cd)



;;; % DERIVED MEASUREMENT UNITS


(defclass derived-measurement-class (measurement-class)
  ())



#|

Issue: Towards a methodology for linear unit conversion

NIST SP 811[1] appendix B defines an effective table of conversion
factors for standard measurement units recognized by the NIST. The
table is defined with a format, effectively: (S,D,F,E) for 
  S: source measurement unit
  D: destination measurement unit
  F: factor for converstion
  E: decimal exponent for factor of conversion

S and D do not appear uniquely in the table, but the set {S,D) is
unique within the 

In transposing that measurement conversions table into this software
system, a number of matters may be considered, for example:

1. That a floating-point value 3.048 E-03 (imay be represented similarly
   as 3048 E-6, with the exponent value stored seperate to the integral
   magnitude -- thus, ensuring that a manner of integral arithmetic
   may be applied onto the integral magnitude and its integral decimal
   degree.

2. that for any measurement converstion path A..D in which the
   following measurement conversion paths are available:
     A..B
     B..C
     C..D
   ...the converstion from A to D may be accomplished directly by
   multiplying the factors for the conversions A..B, B..C, and C..D
   and summing their decimal degrees

   

NIST SP 811[1] clause B.9, furthermore, definesa set of individual
measurement domains and subdomains.





[1] NIST. Guide for the Use of the International System of Units
     http://physics.nist.gov/cuu/pdf/sp811.pdf


|#



;; (defgeneric convert (scalar unit)
;;   (:method (scalar (unit symbol))
;;     (convert scalar (find-unit-using-symbol unit))
;;     ))
                   
  

;; SEE ALSO...
;; http://goldbook.iupac.org/list_math.html
;; esp. http://goldbook.iupac.org/list_goldbook_quantities_defs_A.html


;; referencing http://www.hytime.org/materials/hi2mdhyt.sgm
;;
;; notation names and public identifiers, for 'standard measurement unit'
;; (SMU) definitions...
;;
;; from the HyTime Granule Definition Notation module ...
;;
;; of "ISO/IEC 10744:1997", i.e. Hypermedia/Time-based Structuring
;; Language (HyTime)
;;
;; gQuantum "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"
;; SIsecond "ISO/IEC 10744:1997//NOTATION Systeme International second//EN"
;; SImeter "ISO/IEC 10744:1997//NOTATION Systeme International meter//EN"
;; virTime "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"
;; virSpace "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"
;; SIkg "ISO/IEC 10744:1997//NOTATION Systeme International kilogram//EN"
;; SIcd ""ISO/IEC 10744:1997//NOTATION Systeme International candela//EN"
;; SIampere "ISO/IEC 10744:1997//NOTATION Systeme International ampere//EN"
;; SImole "ISO/IEC 10744:1997//NOTATION Systeme International  mole//EN"
;; SIradian "ISO/IEC 10744:1997//NOTATION Systeme International radian//EN"
;; SIsr "ISO/IEC 10744:1997//NOTATION Systeme International steradian//EN"
;;
;; Issue: The gQuantum, virTime and virSpace notations share the same
;; public idenifier, but may be differentiated by their respective
;; notation names. Though practically useful, however those
;; measurement units are not standardized onto SI
;;
;; see also: 
;; http://crism.maden.org/consulting/pub/hytime/meas.html (1992)
;; http://www.is-thought.co.uk/schedule.htm




;;; % Geometry Domain

;; TO DO

(defclass scalar (measurement) 
  ;; effectively a monadic coordinate
  ())


;;; % Concepts of Plane and Space


;; (defclass plane ...)

;; (defclass euclidian-plane ...)
;; ^ alternate name for cartesian coordinate plane ?(?)
;;
;; (defclass rectangular-plane ...)
;; ^ a coordiate "two space" onto (i, j), complex number system

;; (defclass space ...)
;; ... reference vectors ...

;;; % Concepts of Point and Location

;; The term "Coordinate" will be applied informaly, in the following
;;
;; (defclass diadic-coordinate ...) 
;; ^ a <point> onto a <planar surface>
;;
;; (defclass triadic-coordinate ...) 
;; ^ a <point> onto orthogonal <3 space>
;;
;; (defclass polar-coordinate ...) 
;; ^ a <point> within a <polar coordinate> system  on a <planar surface>
;;
;; (defclass spherical-coordinate ...)
;; ^ a <point> within a <spherical coordinate space>,
;;   using a single reference model for spherical coordinates
;;   cf. RA, AZ, DEC, and broader astrometry

;;; % VECTOR as a formal mathematical object type

;; Note that this sytem will use radian notation for vectors, internally

;; (def-frob* *null-vector* ...)

;; (defun null-vector-p vector ...)

;; (def-frob @= vector vector)
;; (def-frob @+ vector vector)
;; (def-frob @* scalar vector) ;; vector dot product
;; (def-frob @* vector vector) ;; vector cross product

;; (def-frob @=@ &rest objects) ;; tail recursion

;; thence to extend this system onto the integral and differential
;; calculus, with applications for frequency domain analysis of 
;; electronic system components

;; to do: add a seperate CLIM presentations system, focusing on
;; applications within electronic design and analysis,
;; and emphasiziing CLIM's definition of a standard approach for
;; visualization of arbitrary coordinate systems onto a planar
;; display screen
