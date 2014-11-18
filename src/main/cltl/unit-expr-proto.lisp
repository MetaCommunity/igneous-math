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

(defun diadic-measurement-type-query (op u1 u2)
  ;; values: base-unit, u1-adjust-factor, u1-adjust-factor-exponent,
  ;;         u2-adjust-factor, u2-adjust-factor-exponent

  ;; FIXME: Applicable only for diadic operations
  (declare (type measurement-class u1 u2))
  "Find a unit of meaurement expressive of the diadic relation: u1 op u2"
  (cond
    ((eq u1 u2)
     ;; use unit-EQ optimizations
     (ecase op ;; FROB (PROTOTYPE)
       ((+ -) (values u1 1 0 1 0))
       (* (values (find-geometric-measurement-type
		   (measurement-domain-base-measure  (class-of u1))
		   (+ (mesurement-domain-degree u1)
		      (mesurement-domain-degree u2)))
		  ;;  e.g for (@* #m<1 m> #m<1 m>)
		  ?? ??
		  ?? ??
		  ))
       (/ (values 
	   ;; may => #<UNITY ...>
	   (find-geometric-measurement-type
	    (measurement-domain-base-measure (class-of u1))
	    (-1 (mesurement-domain-degree u1)
		(mesurement-domain-degree u2)))
	   ;;  e.g for (@/ #m<1 m> #m<1 m>)
	   ?? ??
	   ?? ??))))
    (t (let ((d-1 (class-of u1))
	     (d-2 (class-of u2)))
	 (cond
	   ((eq d-2 (find-class 'dimensionless-measure))
	    ;; OP: EXPT, LOG
	    )
	   ((eq d-1 d-2)
	    ;; OP : +, -, =, /=, <, >, <=, >=, MAX, MIN,
	    ;;      GCD, LCM, 
	    ;; return values for shifting each of u1 and u2 onto the
	    ;; base mesurement unit of the domain, similar to
	    ;; diadic-domain-analyze
	    )
	   ((eq (measurement-domain-base-domain d-1)
		(measurement-domain-base-domain d-2))
	    ;; OP must be a geometric operation, e.g. *, /, MOD, REM, FLOOR &family, diadic ATAN
	    ;; Return the degree of the subsequent measurement unit
	    ;; resulting from D-1 OP D-1
	    (let ((deg-1 (measurement-domain-degree d-1))
		  (deg-2 (measurement-domain-degree d-2)))
	    ;;; prototype:
	    (ecase op
	      (* (values the-unit-fu (+ deg-1 deg-2)))
	      (/ (values the-unit-fu (- deg-1 deg-2))))))
	   
	    )
	 ))))


#+NIL
(defun compute-measurement-type (op u1 u2)
  ;; FIXME: Functionally redundant onto DIADIC-MEASUREMENT-TYPE-QUERY

  ;; FIXME: Applicable only for diadic operations
  (declare (type measurement-class u1 u2))
  ;; frob - prototype - should dispatch on 'OP'
  )

(defun diadic-domain-analyze (op m1 m2)
  ;; FIXME: Functionally redundant onto DIADIC-MEASUREMENT-TYPE-QUERY

  ;; FIXME: Applicable only for diadic operations

  ;; values: base-unit, u1-factor, u1-factor-exponent,
  ;;         u2-factor, u2-factor-exponent
  (let* ((unit-1 (class-of m1))
	 (unit-2 (class-of m2))
	 (domain-1 (class-of unit-1))
	 (domain-2 (class-of unit-2))
	 (base-1 (measurement-domain-base-measure domain-1))
	 (base-2 (measurement-domain-base-measure domain-2)))
    (let ((unit (diadic-measurement-type-query op base-1 base-2)))
      (values unit 
	      (measurement-base-factor unit-1)
	      (measurement-base-factor-exponent unit-1)
	      (measurement-base-factor unit-2)
	      (measurement-base-factor-exponent unit-2)))))


(labels ((m* (m1 m2)
	   (multiple-value-bind (base-unit m1-factor m1-factor-exponent
					   m2-factor m2-factor-exponent)
	       (diadic-domain-analyze '* m1 m2)
	     (make-expr base-unit
			:factor
			(* (expr-factor m1) m1-factor
			   (expr-factor m2) m2-factor))))
	 (m+ (m1 m2)
	   (multiple-value-bind (base-unit m1-factor m1-factor-exponent
					   m2-factor m2-factor-exponent)
	       (diadic-domain-analyze '+ m1 m2)
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

  ;; FIXME: Testing only for diadic operations
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




;; FIXME: Also develop tests onto monadic operaitons

