;; unit-expr-proto.lisp -- prototyeps for expression of measurement units



(in-package #:math)


#+PROTOTYPE
(defstruct (expr
	    (:constructor make-expr (unit &key (factor 1) (degree 1))))
  (factor 1)
  (unit))

#+PROTOTYPE
(defun domain-analyze (op u1 u2)
  (let* ((unit-1 (expr-unit u1))
	 (unit-2 (expr-unit u2)))
    (multiple-value-bind (base-1 factor-1)
	(factor-for-base-unit unit-1)
      (multiple-value-bind (base-2 factor-2)
	  (factor-for-base-unit unit-2)
	(let ((computed-unit (compute-unit op base-1 base-2)))
	  (values computed-unit factor-1 factor-2))))))


#+PROTOTYPE
(labels ((unit* (u1 u2)
	   (multiple-value-bind (base-unit u1-factor u2-factor)
	       (domain-analyze '* u1 u2)
	     (make-expr base-unit
			:factor
			(* (expr-factor u1) u1-factor
			   (expr-factor u2) u2-factor))))
	 (unit+ (u1 u2)
	   (multiple-value-bind (base-unit u1-factor u2-factor)
	       (domain-analyze '+ u1 u2)
	     (make-expr base-unit
			:factor
			(+ (* (expr-factor u1) u1-factor)
			   (* (expr-factor u2) u2-factor))))))
  (let ((u1 (make-expr :m))
	(u2 (make-expr :m)))
    (values (unit* u1 u2)
	    #+TO-DO (unit+ u1 u2))))

;; ---------

(defun query-measurement-type (op u1 u2)
  (declare (type measurement-class u1 u2))
  "Find a unit of meaurement expressive of the linear relation: u1 op u2"
  (cond
    ((eq u1 u2)
     ;; use unit-EQ optimizations in COMPUTE-MEASUREMENT
     (compute-measurement op u1 u2))
    (t (let ((d-1 (class-of u1))
	     (d-2 (class-of u2)))
	 (cond
	   ((eq d-1 d-2)
	    ;; return values for shifting each of u1 and u2 onto the
	    ;; base mesurement unit of the domain, similar to
	    ;; domain-analyze
	    )
	   )))))

(defun compute-measurement-type (op u1 u2)
  (declare (type measurement-class u1 u2))
  ;; values: base-unit, u1-factor, u1-factor-exponent,
  ;;         u2-factor, u2-factor-exponent

  ;; frob - prototype - should dispatch on 'OP'
  (ecase op
    ((+ -) (if (eq u1 u2)
	       (values u1 1 0 1 0)
	       (query-measurement-type op u1 u2)))
    (* (if (eq u1 u2)
	   (query-measurement-type-dimensionally u1 u2)
	   (query-measurement-type op u1 u2)))
    (/ (if (eq u1 u2)
	   (dimensionless-measurement)
	   (query-measurement-type op u1 u2)))))

(defun domain-analyze (op m1 m2)
  ;; values: base-unit, u1-factor, u1-factor-exponent,
  ;;         u2-factor, u2-factor-exponent
  (let* ((unit-1 (class-of m1))
	 (unit-2 (class-of m2))
	 (domain-1 (class-of unit-1))
	 (domain-2 (class-of unit-2))
	 (base-1 (measurement-domain-base-measure domain-1))
	 (base-2 (measurement-domain-base-measure domain-2)))
    (let ((unit (compute-measurement-type op base-1 base-2)))
      (values unit 
	      (measurement-base-factor unit-1)
	      (measurement-base-factor-exponent unit-1)
	      (measurement-base-factor unit-2)
	      (measurement-base-factor-exponent unit-2)))))


(labels ((m* (m1 m2)
	   (multiple-value-bind (base-unit m1-factor m1-factor-exponent
					   m2-factor m2-factor-exponent)
	       (domain-analyze '* m1 m2)
	     (make-expr base-unit
			:factor
			(* (expr-factor m1) m1-factor
			   (expr-factor m2) m2-factor))))
	 (m+ (m1 m2)
	   (multiple-value-bind (base-unit m1-factor m1-factor-exponent
					   m2-factor m2-factor-exponent)
	       (domain-analyze '+ m1 m2)
	       (let ((m1-m (measurement-magnitude m1))
		     (m1-d (measurement-degree m1))
		     (m2-m (measurement-magnitude m2))
		     (m2-d (measurement-degree m2)))
		 (make-measurement  
		  ;; FROB not handling DEGREE per decimal scaling,
		  ;; not seperate to handling the measurement magnitudes
		  (+ (* m1-m m1-factor 
			(expt 10 m1-factor-exponent) 
			(expt 10 m1-d))
		     (* m2-m m2-factor 
			(expt 10 m2-factor-exponent) 
			(expt 10 m2-d)))
		  base-unit)))))
  (let ((m1 (make-measurement 1 :|m|))
	(m2 (make-measurement 1 :|ft|)))

    (let ((plus-test (m+ m1 m2)))
    (values #+TO-DO (m* m1 m2)

	    plus-test 
	    (float (base-magnitude plus-test ) pi)
	    (= (base-magnitude plus-test)
	       (+ 1 (/ 1200 3937)))
	    )
    
    ;; "CHECK" on the PLUS-TEST
    )))


