;; measurement-ov.lisp - math operations for measurement values
;; [prototype 0]
;;
;; see also: unit-expr-proto.lisp

(in-package #:math)

#+PROTOTYPE-0
(defgeneric unit-degree (measurement-unit)) ;; FIXME: Move to other file

#+PROTOTYPE-0
(defgeneric compute-measurement-class (operation a b)
  ;; avoiding EQL specializers, "as a matter of principle"
  
  ;; @+ => base measurement unit of domain of A, B + conversion
  ;; @- => base measurement unit of domain of A, B + conversion
  ;; @* => exponent (!) of base measurement unit of domain of A, B
  ;;       ...as per the sum of the "unit degree" of each of A, B
  ;;      + conversion
  ;; @/ => "Unity" unit + conversion

  ;; In such unit conversions as must be performed within a
  ;; mathematical operation OP onto measurement objects (A,B), the
  ;; application should prefer to convert to a base unit  of measure,
  ;; per the domains of A,B onto OP.
  ;;
  ;; For A,B subtype LENGTH, and OP being @+ or @- (linear op)
  ;; the result should be of type METER 
  ;; i.e. the base measurement unit of the LENGTH domain
  ;;
  ;; For A,B subtype LENGTH, and OP being @* (geometric op)
  ;; the result should be of type METER^2
  ;; i.e. the square of the base measurement unit
  ;; of the LENGTH measuremnet domain

  ;; e.g for A,B of any type, and OP being @/
  ;; the result should be of type UNITY 

  ;; for A,B of different measurement domains
  ;; ... must query the measurement units database, per (OP, A, B)


  #+NIL
  (:method ((operation math-op) (a measurement) (b measurement))
    (funcall (the function (measurement-unit-selector-function opreation))
	     a b)))

#+PROTOTYPE-0
(defmacro with-values ((a-m a-d b-m b-d base class) (op a b) &body body)
  (with-gensym (%a %b)
  `(let ((,%a ,a)
	 (,%b ,b))
     (declare (type (measurement ,%a ,%b))
	      (inline + - * / expt))
     (let ((,class (compute-measurement-class (function ,op) ,%a ,%b))
	   (,a-m (measurement-magnitude ,%a))
	   (,a-d (measurement-degree ,%a))
	   (,b-m (measurement-magnitude ,%b))
	   (,b-d (measurement-degree ,%b))
	   (,base (measurement-factor-base ,%a)))
       (declare  (type rational ,a-m ,b-m)
		 (type fixnum ,a-d ,b-d ,base))
       ,@body))))


(defmethod %+ ((a measurement))
  (values a))

#+PROTOTYPE-0
(defmethod @+ ((a measurement) (b measurement))
  ;; ISSUE: To ensure that (class-of A) is subtypep (class-of b))
  ;; ISSUE: To determine of which class the resulting measurement
  ;; should be. Arbitrary approach uses A
  (with-values (a-m a-d b-m b-d base class) 
      (@+ a b) 
    (labels ((scale+ (c-m d-m s)
	       (declare (type rational c-m d-m)
			(type fixnum s))
	       (cond ((zerop s)
		      (+ c-m d-m))
		     (t
		      (+ c-m (* d-m (expt base s)))))))
      (multiple-value-bind (m d)
	  (cond ((< a-d b-d)
		 (values (scale+ a-m b-m (- b-d a-d))
			 a-d))
		(t 
		 (values (scale+ b-m a-m (- a-d b-d))
			 b-d)))
	(make-measurement m class d)))))

;; 5*10^2 + 5*10^2  = 10^2(5 + 5)) = 1000
;; (scalar-magnitude (@+ (make-measurement 5 :|m| 2) (make-measurement 5 :|m| 2)))
;; => 1000

;; 5*10^2 + 5*10^3  = 10^2(5*10^0 + 5*10^1) = 5500
;; (scalar-magnitude (@+ (make-measurement 5 :|m| 2) (make-measurement 5 :|m| 3)))
;; => 5500
;; (scalar-magnitude (@+ (make-measurement 5 :|m| 3) (make-measurement 5 :|m| 2)))
;; => 5500

;; 5*10^-2 + 5*10^3 = 10^-2(5*10^0 + 5*10^5) = 100001/20 = 5000.05
;; (scalar-magnitude (@+ (make-measurement 5 :|m| -2) (make-measurement 5 :|m| 3)))
;; => 100001/20
;; (scalar-magnitude (@+ (make-measurement 5 :|m| 3) (make-measurement 5 :|m| -2)))
;; => 100001/20

(defmethod %- ((a measurement))
  (values 
   (make-measurement (- (measurement-magnitude a))
		     (class-of a)
		     (measurement-degree a))))

;; (= (scalar-magnitude (%- (make-measurement 1 :|m|))) -1)
;; => T
;; (= (scalar-magnitude (%- (make-measurement -1 :|m|))) 1)
;; => T


#+PROTOTYPE-0
(defmethod @- ((a measurement) (b measurement))
  ;; ISSUE: To ensure that (class-of A) is subtypep (class-of b))
  ;; ISSUE: To determine of which class the resulting measurement
  ;; should be. Arbitrary approach uses A
  (with-values (a-m a-d b-m b-d base class) 
      (@- a b) 
    (labels ((scale- (c-m d-m s)
	       (declare (type rational c-m d-m)
			(type fixnum s))
	       (cond ((zerop s)
		      (- c-m d-m))
		     (t
		      (- c-m (* d-m (expt base s)))))))
      (multiple-value-bind (m d)
	  (cond ((< a-d b-d)
		 (values (scale- a-m b-m (- b-d a-d))
			 a-d))
		(t 
		 (values (scale- b-m a-m (- a-d b-d))
			 b-d)))
	(make-measurement m class d)))))

;; 5*10^2 - 5*10^2  = 10^2(5 - 5)) = 0
;; (scalar-magnitude (@- (make-measurement 5 :|m| 2) (make-measurement 5 :|m| 2)))
;; => 0

;; 5*10^2 - 5*10^3  = 10^2(5*10^0 - 5*10^1) = -4500
;; (scalar-magnitude (@- (make-measurement 5 :|m| 2) (make-measurement 5 :|m| 3)))
;; => -4500

;; 5*10^-2 - 5*10^3 = 10^-2(5*10^0 - 5*10^5) = -99999/20 = -4999.95
;; (scalar-magnitude (@- (make-measurement 5 :|m| -2) (make-measurement 5 :|m| 3)))
;; => -99999/20


(defmethod %* ((a measurement))
  (values a))

#+NIL ;; FIXME: Must return measurement of type unit^2
(defmethod @* ((a measurement) (b measurement))
  (with-values (@* a-m a-d b-m b-d base class) 
      (a b) 
    (declare (ignore base))
    (make-measurement (* a-m b-m)
		      class
		      (+ a-d b-d))))



(defmethod %/ ((a measurement))
  (values
   (make-measurement (/ (measurement-magnitude a))
		     (class-of a)
		     (- (measurement-degree a)))))

;; (= (scalar-magnitude (%/ (make-measurement 1 :|m|))) 1)
;; => T

;; (= (scalar-magnitude (%/ (make-measurement 10 :|m|))) 1/10)
;; => T

;; (= (scalar-magnitude (%/ (make-measurement 1 :|m| 1))) 1/10)
;; => T


;; FIXME: Must return measurement of a "dimensionless" unit
#+NIL
(defmethod @/ ((a measurement) (b measurement))
  (ERROR "Frob"))
