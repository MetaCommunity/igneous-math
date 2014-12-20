;; measurement.lisp - measurement object model

(in-package #:math)

;; see also:
;; * http://www.bipm.org/en/publications/si-brochure/
;; * http://physics.nist.gov/pubs/sp811/contents.html
;; * IUPAC 'Gold Book' http://goldbook.iupac.org/list_math.html
;;   esp. <http://goldbook.iupac.org/list_goldbook_quantities_defs_A.html>
;; * <http://physics.nist.gov/cuu/units/>
;; * <http://physics.nist.gov/pubs/sp811/appenb.html>
;;    * ^ esp. for formal conversions regarding foot, mile, yard , ...

    

(defgeneric measurement-symbol (instance))


(defgeneric measurement-base-factor (measurement))
(defgeneric measurement-base-factor-exponent (measurement))


;;; % Measurement Class

(defclass* (measurement-class 
	    :conc-name #:measurement-)
    (pretty-printable-object standard-class)
  ((symbol symbol  :read-only t)
   (base-factor 
    real 
    :read-only t
    :initform 1)
   (base-factor-exponent 
    fixnum 
    :read-only t
    :initform 0)
   ))

(validate-class measurement-class)

(deftype measurement-class-designator ()
  '(or symbol measurement-class))

(define-condition measurement-class-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "No measurement class registered for name ~S"
             (entity-condition-name c)))))



;;; %%% Global Measurement Class Storage (Symbol Key)

;;; %%%% Storage

(declaim (type (vector measurement-class) %measurement-classes% ))

(defvar %measurement-classes% (make-array 7 :fill-pointer 0 
			      :element-type 'measurement-class)
  "Internal storage for measurement classes.

This variable should be accessed with `%MEASUREMENT-CLASSES-LOCK%' held")

;;; %%%% Locking (Thread Safety)

(defvar %measurement-classes-lock% (make-lock "%MEASUREMENT-CLASSES%")
  "Mutex lock for accessing `%MEASUREMENT-CLASSES%'")

;;; %%%% Access Functions

(defun register-measurement-class (c)
  (declare (type measurement-class-designator c)
           ;; "Type assertion too complex to check" (SBCL)
           #+NIL (values measurement-class))
  (with-lock-held (%measurement-classes-lock%)
    (let* ((%c (compute-class c))
           (s (measurement-symbol %c))
	   (n (position s %measurement-classes%
			:test #'eq
			:key #'measurement-symbol)))
      (cond 
	(n (unless (eq (aref %measurement-classes% n) %c)
             (simple-style-warning 
              "Redfining measurement class for ~S" s)
             (setf (aref %measurement-classes% n) %c)))
	(t (vector-push-extend %c %measurement-classes%)))
      (let ((base-f (measurement-base-factor %c))
	    (base-f-e (measurement-base-factor-exponent %c)))
	(unless (and (zerop base-f-e)
		     (eql base-f 1))
	  (let* ((domain (class-of %c))
		 (base-m (measurement-domain-base-measure domain))
		 (cf-to (make-conversion-factor  %c  base-m
						 base-f base-f-e))
		 (cf-from (make-conversion-factor base-m %c
						  (/ base-f) 
						  (- base-f-e))))
            (finalize-inheritance domain)
	    (register-measurement-conversion-factor cf-to domain)
	    (register-measurement-conversion-factor cf-from domain))))
      (values %c))))

(defun find-measurement-class (s)
  (declare (type symbol s)
	   (values measurement-class &optional))
  (with-lock-held (%measurement-classes-lock%)
    (or (find s %measurement-classes%
	      :test #'eq
	      :key #'measurement-symbol)
	(error 'measurement-class-not-found :name s))))


;;; % CONVERSION-FACTOR

(defclass* (conversion-factor
	    :conc-name #:factor-)
    ()
  ((source-unit
    measurement-class
    :read-only t)
   (magnitude 
    real
    :read-only t)
   (exponent 
    fixnum
    :initform 0
    :read-only t)
   (destination-unit
    measurement-class
    :read-only t)))

(defmethod print-object ((object conversion-factor) stream)
  (macrolet ((safely (form)
	       `(ignore-errors ,form)))
    (print-unreadable-object (object stream :type t)
      (format stream "(1 ~A) => (~A~@D ~A)"
	      (safely (object-print-label 
		       (factor-source-unit object)))
	      (safely (factor-magnitude object))
	      (or (safely (factor-exponent object))
		  0)
	      (safely (object-print-label
		       (factor-destination-unit object)))))))

(defun make-conversion-factor (source-unit dest-unit
			       factor-magnitude 
			       &optional (factor-exponent 0))
  (declare (type measurement-class source-unit dest-unit)
	   (type real factor-magnitude)
	   (type fixnum factor-exponent))
  (values (make-instance 'conversion-factor
			 :source-unit source-unit
			 :destination-unit dest-unit
			 :magnitude factor-magnitude
			 :exponent factor-exponent)))


(defgeneric find-conversion-factor (source-unit dest-unit domain)
  (:method ((source-unit symbol) (dest-unit symbol)
	    (domain measurement-domain))
    (find-conversion-factor (find-measurement-class source-unit)
			    (find-measurement-class dest-unit)
			    domain))
  (:method ((source-unit measurement-class) (dest-unit measurement-class)
	    (domain measurement-domain))
    (with-lock-held ((measurement-domain-cf-lock domain))
      (let ((factors (measurement-domain-conversion-factors domain)))
	;; This does not completely walk the measurement-domains table,
	;; performing only a cursory, one-off search towards DST.
	;;
	;; If each derived unit, within a single measurement domain,
	;; registers a factor for converting that unit to the domain's
	;; base measure, and the reciptrocal conversion is also
	;; registered, then it would be possible to convert  
	;; between any registered units within a measurement domain,
	;; using only a table of factors
	(or (find source-unit factors
		  :test #'(lambda (u cf)
			    (declare (ignore u))
			    (and (eq source-unit (factor-source-unit cf))
				 (eq dest-unit (factor-destination-unit cf)))))
	    (simple-program-error
	     "No conversion factor available for converting ~S to ~S within ~S"
	     source-unit dest-unit domain))))))

(defgeneric register-measurement-conversion-factor (factor domain)
  (:method ((factor conversion-factor) 
	    (domain measurement-domain))
    (with-lock-held ((measurement-domain-cf-lock domain))
      ;; FIXME: This vector storage model may not be very efficient, 
      ;; onto a "two part" key, contrasted to an EQUALP hash table.
      (let* ((table (measurement-domain-conversion-factors domain))
	     (n  (position factor table
			   :test (lambda (f-in f-test)
				   (and (eq (factor-source-unit f-in)
					    (factor-source-unit f-test))
					(eq (factor-destination-unit f-in)
					    (factor-destination-unit f-test)))))))
	(cond
	  (n (setf (aref table n) factor))
	  (t (vector-push-extend factor table)))))))



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
    (class-of (class-of instance)))
  (:method ((instance measurement-class))
    (class-of instance)))

;; (measurement-domain (make-measurement 1 :|m|))
;; (measurement-domain (find-class 'meter))
;; ^ EQ => OK

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
  (:method ((instance measurement-class))
    (values 10))
  (:method ((instance measurement))
    (measurement-factor-base (class-of instance))))


(defun base-magnitude (m)  
  "Calculate the scalar magnitude of the measurement M for the base
measurement unit of M"
  ;;
  ;; NB. This function implements a numeric conversion on
  ;; a basis of of prefix magnitude. Conversions per ratios of derived
  ;; units must be implemneted seperately.
  
  ;; FIXME: This was designed specifically around base units, and as
  ;; such, assumes that M is already factored to a base unit
  (declare (type measurement m)
           (values real))
  (let* ((deg (measurement-degree m))
	 (unit (class-of m))
	 (expt-base (measurement-factor-base unit))
	 (base-mag  (measurement-magnitude m)))
    (cond 
      ((zerop deg) (values base-mag))
      (t (values (* base-mag
                    (expt expt-base deg)))))))


;; (base-magnitude (make-measurement 1 :|m| 3))
;; => 1000

;; (base-magnitude (make-measurement 1 :|m| -3))
;; => 1/1000

;; (base-magnitude (make-measurement 1/5 :|m|))
;; => 1/5

;; (base-magnitude (make-measurement 1/5 :|m| -3))
;; => 1/5000


(defgeneric scalar-magnitude (scalar)
  (:method ((scalar measurement))
    "Return the magnitude of the SCALAR measurement onto the base unit
for the measurement"
    (base-magnitude scalar)))

;; (scalar-magnitude (make-measurement 1 :|m| -3))
;; => 1/1000

(defmethod measurement-symbol ((instance measurement))
  (measurement-symbol (class-of instance)))

;;; %% Base Measurement and Derived Measurement Classes

(defclass base-measurement-class (measurement-class)
  ())

(defclass derived-measurement-class (measurement-class)
  ())

;;; %% Initialize fundamental measurement domains and base unit classes



(let ((kwd (find-package '#:keyword))
      (md-c (find-class 'measurement-domain))
      (mc-c (find-class 'measurement-class))
      (bm-c (find-class 'base-measurement-class))
      (m-c (find-class 'measurement))
      ;; FIXME: Portable source locations - see also, SLIME/SWANK
      #+SBCL (src (sb-c:source-location)))
  (labels ((do-def (domain domain-name class print-name print-label name)
	     (let* ((b-d-name (intern-formatted "BASE-~A"  domain))
                    (d 
		     (ensure-class 
			domain
			:symbol (intern* domain kwd)
			:print-name domain-name
			:print-label domain-name
			:direct-superclasses (list mc-c)
                        ;; ^ FIXME: This results in all instances of a
                        ;; measurement domain being of type
                        ;; BASE-MEASUREMENT-CLASS - incorrectly.
                        ;;
                        ;; Proposal:
                        ;; For definition of these base measurements,
                        ;; it may be appropriate to define a class
                        ;; BASE-{DOMAIN-NAME} such that would be a
                        ;; subclass of DOMAIN and would have `bm-c` as
                        ;; one of its direct superclasses. Then,
                        ;; derived measurement units may still use
                        ;; {DOMAIN-NAME} as their metaclass, though
                        ;; not being denoted themselves as being of
                        ;; type  BASE-MEASUREMENT-CLASS
                        :documentation 
                        ;; FIXME: I18N
                        (format nil "Measurement domain for quantities of ~A" 
                                domain-name)
			:metaclass md-c
			#+SBCL :definition-source #+SBCL src))
                    (b-d
                     (ensure-class b-d-name
                                   :direct-superclasses (list bm-c 
                                                              domain)
                                   :metaclass md-c))
		    (c 
		     (ensure-class 
		      class
		      :direct-superclasses (list m-c)
		      :symbol name
		      :print-name print-name
		      :print-label print-label
                      :documentation
                      ;; FIXME: I18N
                      (format nil "Measurement class for quanities ~
of ~A in base unit ~A (~A)~2%~
Symbolic representation: ~S"
                              domain-name
                              print-name
                              print-label
                              name)
		      :metaclass b-d
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
            ;; N.B: With regards to normalization of compound unit
            ;; expressions, the set of measurement units defined with
            ;; this form may be stored in a seperate, constant
            ;; (stack allocated) vector of SI base units. The order of
            ;; storage of measurement units within that vector will be
            ;; significant, and must correspond with section 2.1.2 of 
            ;; http://www.bipm.org/en/publications/si-brochure/
            ;;
            ;; Considering the possibly appolications of an "SI Units"
            ;; vector, as such: 
            ;; * The "SI Units" vector may be applied for a normal
            ;;   ordering of elements within compound unit expressions
            ;; * Not only the previous, but the "SI Units" vector may
            ;;   also be aplied in normalization of _partially
            ;;   normalized_ compound unit expressions (e.g. `W A^-1`)
            ;; See also: Documentation, section "Compound unit
            ;; expressions" 
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
;; => #<MEASUREMENT-DOMAIN BASE-LENGTH>

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
  ;; usage case, the class GRAM will remain directly undefined of this
  ;; system. 
  ()
  (:metaclass mass)
  (:print-name . "gram") ;; FIXME: #I18N (EN_UK => gramme)
  (:print-label . "g")
  (:symbol :|g|))

;; (make-measurement 1 :|kg|)
;; => #<KILOGRAM 1000 g {10082A9083}>


;;; % Measurement Initialization


(defun make-measurement (magnitude unit &optional (degree 0))
  "Crate a MEAUREMENT object of the specified MAGNITUDE of a decimal
scale denoted by DEGREE, representing a scalar measurement of
the measurement unit denoted by UNIT. 

Examples:

  (make-measurement 1 :|m|)
  => #<METER 1 m {1006289003}>

 (make-measurement 1 :|m| -3)
 => #<METER 1 mm {10062E90C3}>


See also: 
* `scalar-magnitude'
* `prefix-of'
* `rescale', `nrescale'"
  (declare (type real magnitude)
           (type fixnum degree)
           (type measurement-class-designator unit)
           (values measurement))
  (let ((class (etypecase unit
                   (symbol (find-measurement-class unit))
                   (measurement-class unit))))
    ;; FIXME: This does not explicitly scale the {MAGNITUDE, DEGREE}
    ;; down to the base measurement unit. 

    ;; In an optimized implementation, all measurement values may be
    ;; scaled to integer/magnitude+exponent values onto the base
    ;; measurement unit -- thus, allowing for direct application of
    ;; formulas likewise defined onto the base measurement unit,
    ;; without further magnitude+exponent scaling -- essentially,
    ;; leaving any conversion from/to non-base measurement units to the
    ;; input/display components of the implementation.
    ;;
    ;; Presently, this system applies a mehtodology essentialy of
    ;; "Scaling to significant digits".
    (etypecase magnitude
      (ratio 
       (values (make-instance class :magnitude magnitude 
                              :degree degree)))
      (real 
       (multiple-value-bind (adj-magnitude scale)
           (float-shift-digits magnitude)
         (values (make-instance 
                  class
                  :magnitude adj-magnitude
                  :degree (+ degree scale))))))))

;; (make-measurement 1 :|m| 5)
;; => #<METER 100 km {1003FF93A3}>
;; (measurement-magnitude (make-measurement 1 :|m| 5))
;; => 1

;; Arbitrary instance tests (FIXME: Formalize these instance tests)

;; Example: Scaling to significant digits
;;
;; (measurement-magnitude (make-measurement 103 :|m| 10))
;; => 103
;; (measurement-degree (make-measurement 103 :|m| 10))
;; => 10
;;
;; (measurement-magnitude (make-measurement 1030 :|m| 9))
;; => 103
;; (measurement-degree (make-measurement 1030 :|m| 9))
;; => 10

;;- Ratio magnitude (unscaled)
;; (make-measurement 1/5 :|m|)
;; => #<METER 1/5 m {1003FF93A3}>
;; (measurement-magnitude (make-measurement 1/5 :|m|))
;; => 1/5
;;
;; (make-measurement 1/5 :|m| 3)
;; => #<METER 1/5 km {1007B81023}>
;; (measurement-magnitude (make-measurement 1/5 :|m| 3))
;; => 1/5
;;
;; (scalar-magnitude (make-measurement 1/5 :|m| 3))
;; => 200 ;; i.e. 200 m
;;
;; (scalar-magnitude (make-measurement 1/5 :|m|))
;; => 1/5 ;; i.e. 1/5 m

;; (make-measurement 0.2 :|m|)
;; => #<METER 200 mm {1006CC13C3}>

;; (make-measurement 0.2222 :|m|)
;; => #<METER 222200 μm {1006E29433}>

;; (make-measurement 1 :|kg|)
;; => #<KILOGRAM 1000 g {10065C1043}>

;; (make-measurement 1 :|kg| 6)
;; => #<KILOGRAM 1000 Mg {10066090B3}>

;; (make-measurement 1 :|m| 3)
;; => #<METER 1 km {1003FF93A3}>

;; (measurement-magnitude (make-measurement 1 :|m| 3))
;; => 1
;; (measurement-degree (make-measurement 1 :|m| 3))
;; => 3

;; (measurement-magnitude (make-measurement 1000 :|m| 3))
;; => 1
;; (measurement-degree (make-measurement 1000 :|m| 3))
;; => 6

;; (object-print-label (class-of (make-measurement 1 :|m|)))
;; => "m"


;; % Measurement Unit Expressions (Linear)


(defgeneric unit-expr-unit (expr))
(defgeneric (setf unit-expr-unit) (new-value expr))

(defgeneric unit-expr-degree (expr))
(defgeneric (setf unit-expr-degree) (new-value expr))

(defgeneric unit-expr-elements (expr))
(defgeneric (setf unit-expr-elements) (new-value expr))

(defclass unit-expr ()
  ())

(defclass linear-unit-expr (unit-expr)
  ((unit
    :initarg :unit
    :accessor unit-expr-unit
    :type measurement-class)))
