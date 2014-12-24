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
  ((symbol symbol :read-only t)
   (domain measurement-domain :read-only t)
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

(defmethod shared-initialize :after ((instance measurement-class) slots
                                     &rest initargs 
                                     &key &allow-other-keys)
  (declare (ignore initargs))

  (when (initialize-slot-p 'domain slots)
    (unless (slot-boundp instance 'domain)
      (setf (slot-value instance 'domain)
            (class-of instance))))

  (unless (documentation instance 'type)
    (handler-case
        (with-accessors ((domain measurement-domain)
                         (print-name object-print-name)
                         (print-label object-print-label)
                         (s measurement-symbol)) instance
          (let ((domain-name (object-print-name domain)))
            (setf (documentation instance 'type)
                  (simplify-string 
                   (format nil "Measurement class for quantities of ~A ~
in base unit ~A (~A)~2%~
Symbolic representation: ~S"
                           domain-name
                           print-name
                           print-label
                           s)))))
      (unbound-slot (c)
        (simple-style-warning  "~<Unable to set documentation for ~S~> ~<(~A)~>"
                               instance c)))))


(defmethod domain-of ((instance measurement-class))
  ;; FIXME : Call MEASUREMENT-DOMAIN here?
  (measurement-domain instance))


(deftype measurement-class-designator ()
  '(or symbol measurement-class))

(define-condition measurement-class-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "No measurement class registered for name ~S"
             (entity-condition-name c)))))


(defclass linear-measurement-class (measurement-class)
  ;; This class is defined, here, for purpose of convenience. 
  ;; See also:  
  ;; `GEOMETRIC-MEASUREMENT-CLASS'
  ;; `COMPOUND-MEASUREMENT-CLASS'
  ;; `LINEAR-DERIVED-MEASUREMENT-CLASS'
  ;;
  ;; The concept of whether a measurement class C is a linear,
  ;; geometric, or compound measurement class, essentially, is
  ;; orthogonal to whether C is a base measurement class or derived
  ;; measurement class.
  ;;
  ;; In some pragmatic terms: Every BASE-MEASUREMENT-CLASS is also a
  ;; LINEAR-MEASUREMENT-CLASS, but not every LINEAR-MEASUREMENT-CLASS
  ;; is a BASE-MEASUREMENT-CLASS.
  ;;
  ;; Furthermore, as to whether a linear measurement class defines
  ;; a unit of measure for length: Though there may seem to be some
  ;; ambiguity in the terminology of the object model, but the concept
  ;; of linearity in a measurement class is essentially orthogonal to
  ;; the concept linearity in measurement units. In the first
  ;; instance, the concept of a "linear measurement unit"
  ;; (such as 'm') is contrasted to the concept of a "geometric
  ;; measurement unit" (such as 'm^2' or 'm^3')
  ())


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
      (finalize-inheritance %c)
      (cond 
	(n (unless (eq (aref %measurement-classes% n) %c)
             (simple-style-warning 
              "Redfining measurement class for ~S" s)
             (setf (aref %measurement-classes% n) %c)))
	(t (vector-push-extend %c %measurement-classes%)))
      (let ((base-ftor (measurement-base-factor %c))
	    (base-ftor-exp (measurement-base-factor-exponent %c)))
	(unless (and (zerop base-ftor-exp)
		     (eql base-ftor 1))
	  (let* ((domain (domain-of %c))
		 (base-m (measurement-domain-base-measure domain))
		 (cf-to (make-conversion-factor  %c base-m
						 base-ftor base-ftor-exp))
		 (cf-from (make-conversion-factor base-m %c
						  (/ base-ftor) 
						  (- base-ftor-exp))))
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
      (let ((exp (or (safely (factor-exponent object))
                     0)))
        (format stream "(1 ~A) => (~A~@[ * 10^~D~] ~A)"
                (safely (object-print-label 
                         (factor-source-unit object)))
                (safely (factor-magnitude object))
                (unless (zerop exp) exp)
                (safely (object-print-label
                         (factor-destination-unit object))))))))

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


(defgeneric find-conversion-factor (source-unit dest-unit)
  (:method ((source-unit symbol) (dest-unit symbol))
    (find-conversion-factor (find-measurement-class source-unit)
			    (find-measurement-class dest-unit)))
  (:method ((source-unit measurement-class) (dest-unit measurement-class))
    (let ((domain (verify-conversion-domain source-unit dest-unit)))
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
               source-unit dest-unit domain)))))))

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
* `scalar-magnitude' [?]
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
  ;; `scalar-magnitude' for the measurement [?]
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

(defmethod domain-of ((instance measurement))
  (domain-of (class-of instance)))

(defgeneric measurement-base-measure (instance)
  ;; FIXME: FUNCTION NAME AMBIGUITY
  ;;
  ;; RENAME TO: MEASUREMENT-BASE-UNIT (?)
  (:method ((instance measurement))
    (measurement-domain-base-measure (domain-of instance))))


(defgeneric measurement-factor-base (instance)
  ;; FIXME: FUNCTION NAME AMBIGUITY
  ;;
  ;; RENAME TO: MEASUREMENT-BASE-UNIT-FACTOR-BASE (?)

  ;; Old FIXME: Rename to scalar-factor-base (?) 
  (:documentation
   "Return the base of the degree scale factor of the INSTANCE

See also: 
* `measurement-degree'
* `measurement-magnitude'")
  (:method ((instance measurement-class))
    (values 10))
  (:method ((instance measurement))
    (measurement-factor-base (class-of instance))))


(defmethod measurement-symbol ((instance measurement))
  (measurement-symbol (class-of instance)))

;;; %% Base Measurement and Derived Measurement Classes

(defclass base-measurement-class (measurement-class)
  ())

(defmethod domain-of ((instance base-measurement-class))
    (class-of (class-of instance)))

(defmethod measurement-domain-base-measure ((instance base-measurement-class))
  (values instance))

(defclass derived-measurement-class (measurement-class)
  ())

(defmethod domain-of ((instance derived-measurement-class))
    (class-of (class-of instance)))

(defmethod measurement-domain-base-measure ((instance derived-measurement-class))
  ;; example: derived-length
  (labels ((frob ()
             (dolist (c (class-precedence-list 
                         (class-of instance))
                      nil)
               (let ((bm
                      (when (typep c 'measurement-domain)
                        (measurement-domain-base-measure c))))
                 (when bm (return bm))))))
    (handler-case 
        (cond 
          ((next-method-p) (or (call-next-method) (frob)))
          (t (frob)))
      ;; FIXME: KLUDGE! Hairy subtle kludge.
      ;;
      ;; THIS METHOD SHOULD NOT HAVE TO WORK AROUND UNBOUND SLOTS (FIXME)
      (unbound-slot ()
        (frob)))))

;;; %% Initialize fundamental measurement domains and base unit classes



(let ((kwd (find-package '#:keyword))
      (md-mc (find-class 'measurement-domain))
      (mc-mc (find-class 'measurement-class))
      (bm-mc (find-class 'base-measurement-class))
      (lm-mc (find-class 'linear-measurement-class))
      (dm-mc (find-class 'derived-measurement-class))
      (m-c (find-class 'measurement))
      ;; FIXME: Portable source locations - see also, SLIME/SWANK
      #+SBCL (src (sb-c:source-location)))
  (labels ((do-def (domain domain-name class print-name print-label name)
	     (let* ((b-d-name (intern-formatted "BASE-~A"  domain))
                    (d-d-name (intern-formatted "DERIVED-~A" domain))
                    (d 
		     (ensure-class 
			domain
			:symbol (intern* domain kwd)
                        :base-measure class
			:print-name domain-name
			:print-label domain-name
			:direct-superclasses (list mc-mc)
			:metaclass md-mc
			#+SBCL :definition-source #+SBCL src))
                    (b-d ;; base measurement class for DOMAIN
                     ;; This class serves to allow for a base
                     ;; measurement class to be defined as an instance
                     ;; of a class specific to base measurement
                     ;; units.
                     (ensure-class b-d-name
                                   :domain d
                                   :direct-superclasses 
                                   (list bm-mc lm-mc)
                                   :metaclass domain
                                   #+SBCL :definition-source #+SBCL src))
                    (d-d ;; derived measurement class for DOMAIN (aux)
                     ;;
                     ;; FIXME: There may seem to be a certain
                     ;; ambiguity with regards to the term "Base
                     ;; measure," as being developed in this software
                     ;; system. 
                     ;;
                     ;; Classically, a "Base measure" would be one of
                     ;; the seven SI base measurement units. That
                     ;; sense of the term, "Base measure" should be
                     ;; retained within this system, and not shadowed
                     ;; with any ambiguous applications of the same
                     ;; term.
                     ;;
                     ;; So, this system shall define a concept of
                     ;; "fundamental measure" as with regards to
                     ;; measurement domains. A measurement domain's
                     ;; fundamental measure may be a base unit
                     ;; (e.g. meter, in domain 'length') or a derived
                     ;; unit (e.g. joule, in domain 'energy'). 
                     ;;
                     ;; For every measurement domain defined within an
                     ;; "Accepted standard" (e.g. as published of the
                     ;; BIPM, the NIST, or the IUPAC), there must be
                     ;; one "fundamental unit" (e.g joule) to which
                     ;; any additinal derived units
                     ;; (e.g. horsepower(E)) may be reduced. 
                     ;;
                     ;; Whether a fundamental unit B of a measurement
                     ;; domain A would be defined as a base
                     ;; measurement unit, or defined as a derived
                     ;; measurement unit, that would be essentially
                     ;; orthogonal to the role of B as a fundamental 
                     ;; measurement unit within domain A.
                     ;;
                     ;; Tangentially: The nature of a measurement as
                     ;; a base measurement unit would have some
                     ;; meaning as in a contesxt of calculations on
                     ;; measurement values - see also,
                     ;; `NORMALIZE-UNIT-EXPRESSION' 
                     (ensure-class d-d-name
                                   :domain d
                                   :direct-superclasses 
                                   (list dm-mc  lm-mc)
                                   :metaclass domain
                                   #+SBCL :definition-source #+SBCL src))

		    (c  ;; measurement class denoted by 'class' name
		     (ensure-class 
		      class
		      :direct-superclasses (list m-c)
		      :symbol name
		      :print-name print-name
		      :print-label print-label
		      :metaclass b-d
		      #+SBCL :definition-source #+SBCL src 
		      )))
	       (setf (slot-value d 'base-measure) c)

               (finalize-inheritance d)
               (finalize-inheritance b-d)
               (finalize-inheritance d-d)
               (finalize-inheritance c)
               
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
  (:metaclass derived-mass)
  (:base-factor .  1/1000)
  (:print-name . "gram") ;; FIXME: #I18N (EN_UK => gramme)
  (:print-label . "g")
  (:symbol . :|g|))

(register-measurement-class 'gram)

;; (measurement-domain-base-measure (domain-of (make-instance 'gram)))
;; => #<BASE-MASS KILOGRAM>

(defmethod scale-si :around ((measurement gram) &optional ee-p)
  ;; NB: SCALE-SI GRAM effectively converts the input MEASUREMENT  
  ;; onto the SI base measure for units of mass, namely KILOGRAM.
  ;;
  ;; This behavior may not be reflected for other units of mass.
  ;;    e.g. (scale-si <<1 cal>>)  => <<1 cal>> NOT <<... joule>>
  ;;
  ;; [FIXME: Put this into the documentation]
  ;;
  ;; In the implementation specifically of the measurement units
  ;; KILOGRAM and GRAM, this system endeavors to work around the SI
  ;; convention of KILOGRAM being the base measure of units of
  ;; mass. 
  ;;
  ;;  Concerning applications of measurements of mass:
  ;;
  ;;    Notably, the unit KILOGRAM  is applied in some physical 
  ;;    formulas, such as with regards to specific heat. 
  ;;
  ;;  Concerning applications of the SI prefix system for measurements:
  ;;
  ;;    Orthogonally to the convention, 'kilogram as base measure',
  ;;    GRAM is the measurement unit of mass with prefix 0
  ;;
  ;;
  ;; Specifially with regards to SCALE-SI GRAM: Calling programs
  ;; should ensure appropriate selection of the measurement unit for
  ;; the return value
  ;; 
  ;; i.e. (measurement-domain-base-measure (domain-of #<GRAM 1 g {10075C92B3}>))
  ;;      => #<BASE-MASS KILOGRAM>
  ;;      for MEASUREMENT being of type GRAM
  ;;
  (declare (ignore ee-p))
  (multiple-value-bind (magnitude degree) 
      (call-next-method)
    ;; FIXME; DEGREE not always -
    ;;
    ;; TO DO: define BASE-CONVERT-MEASUREMENT* => magnitude, degree
    (let ((deg-base (- degree 3)))
      (values (shift-magnitude measurement deg-base) 
              deg-base))))

;; (make-measurement 1 :|kg|)
;; =should=> #<KILOGRAM 1 kg {10082A9083}> ;; OK

;; (scale-si (make-measurement 1 :|kg|))
;; => 1, 0 ;; OK - SCALE-SI KILOGRAM

#+NIL
(let ((m (base-convert-measurement (make-measurement 1 :|kg|))))
  (values (measurement-magnitude m)
          (measurement-degree m)))
;; => 1, 0 ;; OK - BASE-CONEVERT-MEASUREMENT 1 KG


;; (make-measurement 1 :|g|)
;; =should=> #<GRAM 1 g {...}> ;; FAIL - PRINT-LABEL GRAM

;; (scale-si (make-measurement 1 :|g|))
;; => 1000, -3 ;; OK - SCALE-SI 1 GRAM => 1000 * 10^-3 KG (SCALED)

;; see also: PRINT-LABEL (MEASUREMENT STREAM)

#+NIL
(let ((m (make-measurement 1 :|g|)))
  (values (measurement-magnitude m)
          (measurement-degree m)))
;; => 1, 0 ;; OK - MAKE-MEASUREMENT, MEASUREMENT INITIALIZATION, MEASUREMENT PROPERTIES

#+NIL
(let ((m (base-convert-measurement (make-measurement 1 :|g|))))
  (values (measurement-magnitude m)
          (measurement-degree m)))
;; => 1/1000, 0 ;; OK - BASE-CONVERT-MEASUREMENT 1 GRAM = 1/1000 KG (SCALED)

;; (make-measurement 1000 :|g|)
;; => #<GRAM 1000 g {10075C92B3}> ;; OK

;; (scale-si (make-measurement 1000 :|g|))
;; => 1000, 0 ;; FAIL - SCALE-SI 1000 GRAM => 1 * 10^0 KG (SCALED)
;; =SHOULD=> 1, 0

#+NIL
(let ((m (base-convert-measurement (make-measurement 1000 :|g|))))
  (values (measurement-magnitude m)
          (measurement-degree m)))
;; => 1, 0 ;; OK - BASE-CONVERT-MEASUREMENT 1000 GRAM => 1 KG (SCALED)



;; (convert-measurement (make-measurement 1 :|kg|)  :|g|)
;; =should=> #<GRAM 1000 g {...}> ;; FAIL

;; (convert-measurement (make-measurement 1000 :|g|)  :|kg|)
;; =should=> #<KILOGRAM 1 kg {...}> ;; FAIL - PRINT-LABEL MEASUREMENT STREAM ?

;; (measurement-magnitude (convert-measurement (make-measurement 1 :|kg|)  :|g|))
;; => 1
;; (measurement-degree (convert-measurement (make-measurement 1 :|kg|)  :|g|))
;; => 3 ;; OK

;; (base-convert-measurement (make-measurement 1 :|g|))
;; =should=> #<KILOGRAM 1 g {...}> ;; OK

;; (measurement-magnitude (base-convert-measurement (make-measurement 1 :|g|)))
;; => 1/1000 ;; OK 

;; (base-magnitude (make-measurement 1 :|g|))
;; =should=> 1/1000 ;; OK


;; (find-conversion-factor :|g| :|kg|)
;; => #<CONVERSION-FACTOR (1 g) => (1/1000 kg)> ;; OK
;; (find-conversion-factor :|kg| :|g|)
;; => #<CONVERSION-FACTOR (1 kg) => (1000 g)> ;; OK


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
* `scalar-magnitude' [?]
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
;; (base-magnitude (make-measurement 1/5 :|m| 3))
;; => 200 ;; i.e. 200 m
;;
;; (base-magnitude (make-measurement 1/5 :|m|))
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

;; (object-print-name (class-of (make-measurement 1 :|m|)))


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
