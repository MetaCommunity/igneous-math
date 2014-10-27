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

(defconstant* %integer-instance-classes%
  (let ((c (compute-end-classes (find-class 'integer))))
    #+CCL (cons '(find-clas fixnum) c)
    #-CCL c))

(defconstant* %float-instance-classes%
  (compute-end-classes (find-class 'float)))

(defconstant* %rational-instance-classes%
  (cons (find-class 'ratio)
	%integer-instance-classes%))

(defconstant* %complex-instance-classes%
  (compute-end-classes (find-class 'complex)))

(defconstant* %numeric-instance-classes%
    (append %rational-instance-classes%
	    %float-instance-classes%))


;;; % Overloading for Comutative Functions

(defun defop (op &key 
                   (classes %numeric-instance-classes%)
                   (default (find-class 'number))
		   (monadic-p t)
		   (diadic-p t)
                   (variadic-p t))
  (declare (type symbol op)
           (type (or class-designator null) default)
           (type cons classes)
           (values (or generic-function null)
                   (or generic-function null)
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

	     (defop-error (message args)
	       (simple-program-error "~<[DEFOP ~s]~> ~<~?:~>" 
				     op message args))

	     (require-monadic (message &rest args)
	       (unless monadic-p (defop-error message args)))

	     (require-diadic (message &rest args)
	       (unless diadic-p (defop-error message args)))

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

      (let* ((monadic-op-name (when monadic-p
				(intern (format nil "%~A" op))))
	     (gf-monadic  (when monadic-p
			    (ensure-gfn monadic-op-name '(a))))
	     (diadic-op-name (when diadic-p
			       (intern (format nil "@~A" op))))
	     (gf-diadic (when diadic-p
			  (ensure-gfn diadic-op-name  '(a b)))))
        
	(dolist (c classes)
	  (let* ((%c (compute-class c)))
	    ;; Define methods for diadic and monadic functions
	    ;; specialized onto each C
	    (when diadic-p
	      (makem gf-diadic op (list %c %c) '(a b)
		     `(funcall (function ,op) a b)))
	    (when monadic-p
	      (makem gf-monadic op (list %c) '(a)
		     `(funcall (function ,op) a)))))
	
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
	  ;; FIXME: #I18N
	  (require-diadic "Cannot define DEFAULT diadic method for DEFOP with :DIADIC-P NIL")
	  (let ((%c (compute-class default)))
	    (makem gf-diadic op (list %c %c) '(a b)
		   `(funcall (function ,op) a b))))

	(cond
	  (variadic-p
	   ;; FIXME: #I18N
	   (require-diadic "VARIADIC-P specified for DEFOP with :DIADIC-P NIL")
	   (require-monadic "VAIRADIC-P specified for DEFOP with :MONADIC-P NIL")
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

(defop 'gcd :classes %integer-instance-classes%)
(defop 'lcm :classes %integer-instance-classes%)

;;; % Overloading for Strictly Diadic, Non-Comutative Functions

;;; %% Exponentiation with arbitrary degree

(labels ((defop-2 (name)
	   (defop name :monadic-p nil :diadic-p t :variadic-p nil)))

  (defop-2 'expt )

  ;; (@expt 2 2)
  ;; => 4
  
;;; %% MOD, REM
  
  (defop-2 'mod)
  (defop-2 'rem))

;;; % Overloading for Alternately Monadic/Diadic Functions

;;; %% FLOOR, FFLOOR &FAMILY


(defop 'floor :variadic-p nil)
(defop 'ffloor :variadic-p nil)

;; (%ffloor 1)
;; => 1.0, 0
;;
;; (@ffloor@ 2.0 2.0)
;; => 1.0, 0.0
;;
;; (@ffloor@ 2.0 2.0d0)
;; => 1.0D0, 0.0D0

;; %% ATAN

(defop 'atan :variadic-p nil)

;; (= (/ pi 4) (%atan 1d0))
;; => T

;; (= (/ pi 4) (@atan 2d0 2d0))
;; => T

;; also
;; (= (rationalize (/ pi 4d0)) (rationalize (@atan 2d0 2d0)))
;;
;; although
;; (= (/ (rationalize pi) 4) (rationalize (@atan 2d0 2d0)))
;; => NIL
;;
;; however
;; (= (/ (rational pi) 4) (rational (@atan 2d0 2d0)))
;; => T
;;
;; furthermore
;; (= (rationalize (/ pi 4d0)) (rationalize (@atan 2d0 2d0)))
;; => T
;;
;; thus illustrating some of the contrasting qualities of CL:RATIONAL 
;; and CL:RATIONALIZE - onto that simple wrapper for diadic ATAN

;; Considering the examples in the previous, it may seem to be
;; advisable to apply a methodology of using DOUBLE-FLOAT values,
;; internally, with RATIONALIZE applied when a rational return value
;; is sought?

;; furthermore:
;;
;; (defun rad-to-deg (r) (* r #.(/ 180 (rationalize pi))))
;; 
;; double-float=>rational=>..=>ratio onto pi/4 via effective (atan 1d0)
;; (rad-to-deg (rational (atan 2d0 2d0)))
;; => <large ratio>
;;
;; double-float=>rational=>...=>ratio=>single-float onto pi/4 via effective (atan 1d0)
;; (float (rad-to-deg (rational (atan 2d0 2d0))))
;; => 45.0
;;
;; furthermore, redefining RAD-TO-DEG
;;
;; (defun rad-to-deg (r) (rational (* r #.(/ 180d0 pi))))
;;; ^ double-float => rational (consistently)
;;
;; (rad-to-deg (/ pi 4))
;; => 45
;;
;; lastly:
;;
;; (defun rad-to-deg (r) (rationalize (* r #.(/ 180d0 pi))))
;; (rad-to-deg (/ pi 4))
;; => 45
;;
;; The function RAD-TO-DEG is applied as an example, in this instance,
;; considering the common practice of denoting degree type phase 
;; angles within methodologies of electrical circuit analysis onto
;; inductive, capacitive, and resistive circuits with AC electrical
;; components -- although Common Lisp uses radians, canonically.
;; 


;; %% LOG

(defop 'log :variadic-p nil)

;;; % Overloading for Other Monadic Functions

;; FIXME : Observing the implementation of the respetive CL functions
;; in CCL -- e.g. cl:minusp -- it seems that some optimization is being
;; lost in the broad dispatching on NUMBER. Ideally, this
;; implementation would present all possible optimizations to the
;; compiler

;;; %% Miscellaneous Monadic Functions

(labels ((defop-monadic (op &optional (classes %numeric-instance-classes%))
	   (defop op :diadic-p nil :variadic-p nil
		  :default nil :classes classes)))

;;; %%% EXP

  (defop-monadic 'exp)

;;; %%% SIGNUM

  (defop-monadic 'signum)

;;; %%% SQRT, ISQRT

  (defop-monadic 'sqrt)
  (defop-monadic 'isqrt %integer-instance-classes%)
  
;;; %%% CIS

  (defop-monadic 'cis)

;;; %%% CONJUGATE

  ;: FIXME: Note for possible relevance within AC circuit analysis 
  ;; (RC / RL)
  
  (defop-monadic 'conjugate)

;;; %%% PHASE

  (defop-monadic 'phase)

;;; %%% Structural Accessor Functions

;;; %%%% REALPART, IMAGPART
  
  ;; NB: This is in leaving all of the optimization to the
  ;; implementation -- including any behaviors in numeric type
  ;; handling. In evaluation of DEFOP, notably the numeric OP is
  ;; declared as INLINE within each respective method lambda.
  ;;
  ;; Of course (%REALPART REAL) => REAL
  ;;           (%IMAGPART REAL) => ZERO

  (defop-monadic 'realpart) 
  (defop-monadic 'imagpart)

;;; %%%% NUMERATOR, DENOMINATOR
  
  (defop-monadic 'numerator %rational-instance-classes%)
  (defop-monadic 'denominator %rational-instance-classes%)

;;; %% Overloading for Monadic Predicate Functions

;;; %%% "Number-Line" Predicates

  (defop-monadic 'minusp)
  (defop-monadic 'plusp)
  (defop-monadic 'zerop)

;; %%% EVENP, ODDP

  (defop-monadic 'evenp)
  (defop-monadic 'oddp)


;;; %% Overloading for Monadic Increment Functions

;;; 1+, 1-

  (defop-monadic '1+)
  (defop-monadic '1-)

;;; INCF, DECF (?)

  ;; FIXME: Define overloaded macro forms for INCF, DECF
  ;; using GET-SETF-EXPANSION in each - possibly, applying %1+ and %1-
  ;; within the respective macroexpansions


;;; %% Overloading for Strictly Monadic Transcendental Functions

;; %%% SIN, COS, TAN

  (defop-monadic 'sin)
  (defop-monadic 'cos)
  (defop-monadic 'tan)

;; %%% ASIN, ACOS

  (defop-monadic 'asin)
  (defop-monadic 'acos)

  )

;;; % New Functions

(defgeneric geometric-sum (a b)
  (:generic-function-class monotonic-generic-function)
  (:method ((a number) (b number))
    ;; FIXME: This completely looses optimizations,
    ;; though it does allow for overloaded math operations
    (%sqrt (@+ (@expt (coerce a 'double-float)
		      2d0) 
	       (@expt (coerce b 'double-float)
		      2d0)))))

;; (geometric-sum 3 4)
;; => 5.0d0

;; (geometric-sum 3d0 4d0)
;; => 5.0d0

;; Host library integration in [SBCL]
;
;; Note the applications of foreign-function calls into the host's
;; floating point library, namely within #p"sbcl:src;code;irrat"
;;
;; ...used in the instance of such as (EXPT DOUBLE-FLOAT DOUBLE-FLOAT)
;;
;;
;; If applicable, the DEF-MATH-RTN calls within that file may be
;; called, instead, directly onto the native VFP[+NEON] (armhf)  or
;; SSE2 (intel) hardware.

;; regarding reading of floating point values,
;; on a sidebar, see also SB-IMPL::MAKE-FLOAT
