
(in-package #:math)

;; referencing http://www.bipm.org/en/publications/si-brochure/
;; and http://physics.nist.gov/pubs/sp811/contents.html
    

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
