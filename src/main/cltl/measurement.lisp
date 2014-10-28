
(in-package #:math)

;; see also http://www.bipm.org/en/publications/si-brochure/
;; and http://physics.nist.gov/pubs/sp811/contents.html
    

(defgeneric measurement-symbol (instance))


;;; % Measurement Class

(defclass* (measurement-class 
	    :conc-name #:measurement-)
    (pretty-printable-object standard-class)
  ((symbol symbol  :read-only t)))

(validate-class measurement-class)

(deftype measurement-class-designator ()
  '(or symbol measurement-class))

(define-condition measurement-class-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "No measurement class registered for name ~S"
             (entity-not-found-name c)))))


;;; %% Measurement Class Storage and Access

;;; %%% Locking (Thread Safety)

(declaim (type (vector measurement-class) %measurement-classes% ))

(defvar %measurement-classes% (make-array 7 :fill-pointer 0 
			      :element-type 'measurement-class)
  "Internal storage for measurement measurement-classes.

This variable should be accessed with `%MEASUREMENT-CLASSES-LOCK%' held")


(defvar %measurement-classes-lock% (make-lock "%MEASUREMENT-CLASSES%")
  "Mutex lock for accessing `%DOMAINS%'")


;;; %%% Access Functions

(defun register-measurement-class (c)
  (declare (type measurement-class c))
  (with-lock-held (%measurement-classes-lock%)
    (let* ((s (measurement-symbol c))
	   (n (position s %measurement-classes%
			:test #'eq
			:key #'measurement-symbol)))
      (cond 
	((and n (not (eq (aref %measurement-classes% n) c)))
	 (simple-style-warning 
	  "Redfining measurement class for ~S" s)
	 (setf (aref %measurement-classes% n) c))
	(t (vector-push-extend c %measurement-classes%)))
      (values c))))

(defun find-measurement-class (s)
  (declare (type symbol s)
	   (values measurement-class &optional))
  (with-lock-held (%measurement-classes-lock%)
    (or (find s %measurement-classes%
	      :test #'eq
	      :key #'measurement-symbol)
	(error 'measurement-class-not-found :name s))))

  

;;; % MEASUREMENT

(defgeneric measurement-magnitude (instance)
  (:documentation
   "Return the scalar magnitude of the INSTANCE

See also:
* `scalar-magnitude'
* `measurement-degree'
* `measurement-factor-base'"))
(defgeneric (setf measurement-magnitude) (new-value instance))

(defgeneric measurement-degree (instance)
  (:documentation 
   "Return the degree of the scale factor of the INSTANCE.

See also:
* `measurement-factor-base'
* `measurement-magnitude'"))
(defgeneric (setf measurement-degree) (new-value instance))


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
   (degree fixnum :initform 0)))


(defgeneric measurement-domain (instance)
  (:method ((instance measurement))
    (class-of (class-of instance))))

(defgeneric measurement-base-measure (instance)
  (:method ((instance measurement))
    (measurement-domain-base-measure
     (measurement-domain instance))))


(defgeneric measurement-factor-base (instance)
  ;; FIXME: Rename to scalar-factor-base
  (:documentation
   "Return the base of the degree scale factor of the INSTANCE

See also: 
* `measurement-degree'
* `measurement-magnitude'")
  (:method ((instance measurement))
    (values 10)))


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
      ((zerop deg) (values (measurement-magnitude m)))
      (t (values (* (measurement-magnitude m)
                    (expt (measurement-factor-base m) 
                          deg)))))))

;; (base-magnitude (make-measurement 1 :m 3))
;; => 1000

;; (base-magnitude (make-measurement 1 :m -3))
;; => 1/1000

;; (base-magnitude (make-measurement 1/5 :m))
;; => 1/5

;; (base-magnitude (make-measurement 1/5 :m -3))
;; => 1/5000


(defgeneric scalar-magnitude (scalar)
  (:method ((scalar measurement))
    "Return the magnitude of the SCALAR measurement onto the base unit
for the measurement"
    (base-magnitude scalar)))

;; (scalar-magnitude (make-measurement 1 :m -3))
;; => 1/1000

(defmethod measurement-symbol ((instance measurement))
  (measurement-symbol (class-of instance)))

;;; %% Initialize core measurement domains and base unit classes

(let ((kwd (find-package '#:keyword))
      (md-c (find-class 'measurement-domain))
      (mc-c (find-class 'measurement-class))
      (m-c (find-class 'measurement))
      #+SBCL (src (sb-c:source-location)))
  (labels ((do-def (domain domain-name class print-name print-label name)
	     (let* ((d 
		     (ensure-class 
			domain
			:symbol (intern* domain kwd)
			:print-name domain-name
			:print-label domain-name
			:direct-superclasses (list mc-c)
			:metaclass md-c
			#+SBCL :definition-source #+SBCL src))
		    (c 
		     (ensure-class 
		      class
		      :direct-superclasses (list m-c)
		      :symbol name
		      :print-name print-name
		      :print-label print-label
		      :metaclass d
		      #+SBCL :definition-source #+SBCL src 
		      )))
	       (setf (slot-value d 'base-measure) c)
	       (register-measurement-domain d)
	       (register-measurement-class c)
	       (values d c))))

    (mapcar (lambda (spec)
	      (destructuring-bind 
		    (domain domain-name class print-name print-label  name) spec
		(multiple-value-bind (d c)
		    (do-def domain domain-name class print-name print-label  name)
		  (cons d c))))
	    '((length "length" meter "metre" "m" :|m|)
	      (mass "mass" kilogram "kilogram" "kg" :|kg|)
	      (time "time, duration" second "second" "s" :|s|)
	      (electrical-current "electric current" ampere "ampere" "A" :|a|)
	      (temperature "thermodyamic temperature" kelvin "kelvin" "K" :|k|)
	      (amount-substance "amount of substance" mole "mole" "mol" :|mol|)
	      (luminous-intensity "luminous intensity" candela "candela" "cd" :|cd|)))
    ))

;; (eq (find-class 'length) (class-of (find-class 'meter)))
;; => T
;;
;; (typep (find-class 'meter) 'measurement-class)
;; => T
;;
;; (measurement-domain (make-instance 'meter))
;; => #<MEASUREMENT-DOMAIN LENGTH>

;; (find-measurement-class :|m|)
;; (find-measurement-class :|kg|)
;; (find-measurement-class :|s|)
;; (find-measurement-class :|a|)
;; (find-measurement-class :|k|)
;; (find-measurement-class :|mol|)
;; (find-measurement-class :|cd|)



#+TO-DO?
(defclass gram (kilogram)
  ;; Kilogram is the base unit of measurement for mass, under the
  ;; Systeme International.
  ;;
  ;; The method PRINT-LABEL (KILOGRAM T) will ensure that an
  ;; appropriate magnitude and unit will be printed for KILOGRAM
  ;; measurements.
  ;;
  ;; Though it may be more semantically consistent, if to ensure that
  ;; a class, GRAM, is defined in parallel to KILOGRAM, however
  ;; insofar as that such application presently lacks a necessary
  ;; usage case, the class GRAM will remain effectively undefined.
  ()
  (:metaclass mass)
  (:print-name . "gram") ;; FIXME: #I18N (EN_UK => gramme)
  (:print-label . "g")
  (:symbol :|g|))

;; (make-measurement 1 :|kg|)
;; => #<KILOGRAM 1000 g {10082A9083}>


;;; % DERIVED MEASUREMENT UNITS

#+NIL
(defclass derived-measurement (measurement)
  ;; ???
  ())



#| Topic: Measurement Formulas

The measurement unit, ohm, as a standard unit for measurement of
electrial resistance, is defined as a derived unit with formulas

  1 ohm = 1 m^2 kg s^-3 A^-2	[1] p. 111	i.e. ((:|m| 2) :|kg| (:|s| -3) (:A -2))
  1 ohm = 1 V / 1 A		[1] p. 118

Similarly, the measurement unit, volt, as a standard unit for
measurement of electromotive force or difference in electrical
potential, is defined with formulas

  1 volt = 1 m^2 kg s^-3 A^-1	[1] p. 118
  1 volt = 1 W / 1 A		[1] p. 118

and the measurement unit, watt, as a standard unit for measurement of
power or radiant flux:

 1 watt = 1 m^2 kg s^-3		[1] p. 118
 1 watt = 1 J / 1 S		[1] p. 118

and the measurement unit, joule, as a standard unit for measurement of
work, energy, or amount of heat:

 1 joule = 1 m^2 kg s^-2	[1] p. 118
 1 joule = 1 N * 1 M		[1] p. 118

and the measurement unit, newton, as a standard unit for force:

 1 newton = 1 m kg s^-2		[1] p. 118

Thus, a definition of the derived measurement unit, ohm, effectively
requires the  definitions of the derived measurement units: newton,
joule, watt, and volt.


  [1] http://www.bipm.org/en/publications/si-brochure/
|#

#| Topic: Measurement Converstion

Issue: Towards a methodology for linear unit conversion

NIST SP 811[1] appendix B defines an effective table of conversion
factors for standard measurement units recognized by the NIST. The
table is defined with a format, effectively: (S,D,F,E) for 
  S: source measurement unit
  D: destination measurement unit
  F: factor for converstion
  E: decimal exponent for factor of conversion

S and D do not appear uniquely in the table, but the set {S,D) is
unique within the table.

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


