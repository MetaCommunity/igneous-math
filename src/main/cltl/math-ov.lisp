;; math-ov.lisp : definition of 'overloaded' mathematical operations

(in-package #:math)


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun compute-end-classes (c)
  ;; compute list of "end classes of C"
  ;; i.e. classes being subclasses of C, with no subclasses
  (let ((cds (class-direct-subclasses c)))
    (cond
      (cds (mapcan #'compute-end-classes (copy-list cds)))
      (t (list c)))))
)

(defconstant* %numeric-instance-classes%
    (let ((the-end-classes
           (compute-end-classes (find-class 'number))))
      #+CCL
      (cons (find-class 'fixnum) the-end-classes)
      #-CCL
      the-end-classes))


;;; % Overloading for Comutative Functions

(defun defop (op &key 
                   (classes %numeric-instance-classes%)
                   (default (find-class 'number))
                   (variadic-p t))
  (declare (type symbol op)
           (type (or class-designator null) default)
           (type cons classes)
           (values generic-function
                   generic-function
                   (or generic-function null)))
  "For a funtion <OP> acccepting zero or more arguments, define and
return generic functions: 

  %<OP> (A)

  @<OP> (A B)

  @<OP>@ (&REST VALUES)

For each class C in CLASSES, then define methods:

  %<OP> ((A C))
  evaluating (<OP> A) with A declared as type C

  @<OP> ((A C) (B C))
  evaluating (<OP> A B) with A and B declared as type C

  @<OP>@ (&REST VALUES)
  performing a recrusive evaluation of <OP> onto VALUES,
  using each of @<OP> and %<OP>
"
  

  ;; NB: Though this allows for compier optimizations on numeric
  ;; classes, but it will not allow for optimizations onto types
  ;; derived of numeric classes, e.g. signed/unsigned values

  (let (#+SBCL (src (sb-c:source-location)))
    (labels ((mklambda  (ll op specializers form)
               `(lambda ,ll 
                  (declare (inline ,op)
                           ,@(when specializers
                                   (mapcar #'(lambda (arg c)
                                               `(type ,(class-name c) ,arg))
                                           ll specializers)))
                  ,form))

	     (ensure-gfn (name lambda &optional (prec lambda))
	       (ensure-generic-function 
		name
		:lambda-list lambda
		:argument-precedence-order prec
		:generic-function-class (find-class 'monotonic-generic-function)
		#+SBCL :definition-source #+SBCL src))

             (makem (gf op specializers lambda form)
               (let* ((lform (mklambda lambda op specializers form))
		      (m (make-instance (generic-function-method-class gf)
					:specializers specializers
					:lambda-list lambda
					#+SBCL :definition-source #+SBCL src
					:function 
					;; FIXME: Catch errors/warnings
					;; from COMPILE
					(compile nil 
						 (compute-method-lambda gf lform nil)))))

                 (add-method gf m))))

      (let* ((monadic-op-name (intern (format nil "%~A" op)))
	     (gf-monadic (ensure-gfn monadic-op-name '(a)))
	     (diadic-op-name (intern (format nil "@~A" op)))
	     (gf-diadic (ensure-gfn diadic-op-name  '(a b))))
        
	(dolist (c classes)
	  (let* ((%c (compute-class c)))
	    ;; Define methods for diadic and monadic functions
	    ;; specialized onto each C
	    (makem gf-diadic op (list %c %c) '(a b)
		   `(funcall (function ,op) a b))
	    (makem gf-monadic op (list %c) '(a)
		   `(funcall (function ,op) a))))
	
	(when default 
	  ;; Define a diadic method specialized onto DEFAULT
	  ;;
	  ;; NB: In some implementations, this "default" method may
	  ;; effectively loose compiler optimizations, such that may be
	  ;; available for some instances of A and B delcared
	  ;; explicitly for their respective numeric types -- moreover,
	  ;; in instances when the classes of A and B are not EQ.
	  ;;
	  ;; With OP declared incline, however, maybe it would be
	  ;; possible for the compiler to further optimize the following
	  ;; form.
	  (let ((%c (compute-class default)))
	    (makem gf-diadic op (list %c %c) '(a b)
		   `(funcall (function ,op) a b))))

	(cond
	  (variadic-p
	   ;; Define a variadic function.
	   ;;
	   ;; FIXME: This produces a generic function that canot be
	   ;; any further specialized
	   (let* ((variadic-op-name (intern (format nil "@~A@" op)))
		  (gf-variadic 
		   (ensure-gfn variadic-op-name '(&rest values) nil)))

	     (makem gf-variadic op nil '(&rest values)
		    `(progn
		       (unless (consp values)
			 (simple-program-error 
			  `,(format* "~A called with no arguments"
				     (quote ,variadic-op-name))))
		       (let ((rest (cdr values)))
			 (cond
			   (rest 
			    (funcall (function ,diadic-op-name)
				     (car values)
				     (apply (function ,variadic-op-name)
					    rest)))
			   (t (funcall (function ,monadic-op-name)
				       (car values)))))))

	     (values gf-diadic gf-monadic gf-variadic)))
	  (t (values gf-diadic gf-monadic nil)))))))

;;; % Overloading for Monadic/Diadic/Variadic Functions

;;; %% Nary Basic Math Operations

;; %+ @+ @+@
;; %- @- @-@
;; %* @* @*@
;; %/ @/ @/@

(defop '+)
(defop '-)
(defop '*)
(defop '/)

;; single instance test, ensure +
;; (%+ 1)
;; => 1
;; single instance test, ensure +
;; (@+ 1 1)
;; => 2

;; single instance test, ensure +
;; (@+@ 1 2 3)
;; => 6

;; FIXME : run unit tests on each operation, for each %NUMERIC-END-CLASSES%

;; single instance test, ensure error
;; (@+@)


;;; %% Nary Comparison Functions

(defop '=)
(defop '/=)
(defop '<)
(defop '>)
(defop '<=)
(defop '>=)

;;; %% MAX, MIN

(defop 'max)
(defop 'min)

;; redefine @max@ to apply @> internally
(defmethod @max@ (&rest values)
  (let ((top (car values))
	(stack (cdr values)))
    (cond
      (stack
       (let* ((stack-max (apply #'@max stack))
	      (top-gt-p (@> top stack-max)))
	 (if top-gt-p top stack-max)))
      (t top))))

;; (%max 1)
;; => 1
;; (@max 2 1)
;; => 2
;; (@max@ 1 3 2)
;; => 3

;; redefine @min@ to apply @< internally
(defmethod @min@ (&rest values)
  (let ((top (car values))
	(stack (cdr values)))
    (cond
      (stack
       (let* ((stack-min (apply #'@min stack))
	      (top-lt-p (@< top stack-min)))
	 (if top-lt-p top stack-min)))
      (t top))))

;; (%min 1)
;; => 1
;; (@min 2 1)
;; => 1
;; (@min@ 3 1 2)
;; => 1

;;; % GCD, LCM

;;; % Overloading for Strictly Diadic, Non-Comutative Functions

;;; %% Exponentiation with arbitrary degree

(defgeneric @expt (a b)
  (:method ((a number) (b number))
    (expt a b)))

;;; %% MOD, REM

;;; % Overloading for Alternately Monadic/Diadic Functions

;;; %% FLOOR, FFLOOR &FAMILY

;; FIXME: Possibly loosing more optimizations, here.
;; When possible, try to call directly to the underlying machine
;; instructions (Architecture-specific code - cf. SSE, etc)

(let ((integer-classes 
       (compute-end-classes (find-class 'integer))))
  (dolist (name '(floor ceiling truncate round ))
    (defop name 
        :classes 
      #-CCL integer-classes
      #+CCL (cons (find-class 'fixnum) integer-classes))))

(let ((float-classes 
       (compute-end-classes (find-class 'float))))
  (dolist (name '(ffloor fceiling ftruncate fround ))
    (defop name  :classes float-classes
           :default (find-class 'float))))

;; (%ffloor 1.0)
;; => 1.0, 0.0
;;
;; (@ffloor@ 2.0 2.0)
;; => 1.0, 0.0
;;
;; (@ffloor@ 2.0 2.0d0)
;; => 1.0D0, 0.0D0

;; %% ATAN

;; %% LOG

;;; % Overloading for Other Monadic Functions

;; FIXME : Observing the implementation of the respetive CL functions
;; in CCL -- e.g. cl:minusp -- it seems that some optimization is being
;; lost in the broad dispatching on NUMBER. Ideally, this
;; implementation would present all possible optimizations to the
;; compiler

;;; %% Miscellaneous Monadic Functions

;;; %%% EXP

;;; %%% SIGNUM

;;; %%% SQRT, ISQRT

;;; %%% CIS

;;; %%% CONJUGATE

;;; %%% PHASE

;;; %%% Structural Accessor Functions

;;; %%%% REALPART, IMAGPART

;;; %%%% NUMERATOR, DENOMINATOR


;;; %% Overloading for Monadic Predicate Functions

;;; %%% "Number-Line" Predicates

(defgeneric %minusp (a)
  (:method ((a number))
    (minusp a)))

(defgeneric %plusp (a)
  (:method ((a number))
    (plusp a)))

(defgeneric %zerop (a)
  (:method ((a number))
    (zerop a)))

;; %%% EVENP, ODDP

;;; %% Overloading for Monadic Increment Functions

;;; 1+, 1-
;;; INCF, DECF (?)

;;; %% Overloading for Strictly Monadic Transcendental Functions

;; %%% SIN, COS, TAN
;; %%% ASIN, ACOS


