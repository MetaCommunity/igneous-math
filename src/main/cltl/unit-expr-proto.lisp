;; uxpr.lisp -- prototyeps for expression of measurement units



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
