;; math-ov.lisp : definition of 'overloaded' mathematical operations

(in-package #:math)

;; FIXME: This code does not evalute successfully in SBCL 
;; but this does evaluate successfully in CCL
;;
;; Versions tested
;; * SBCL 1.2.1 (MSWin x86-64)
;; * SBCL 1.2.3 (Linux x86-64)
;; * CCL 1.9-r15765 (windows x86-64)
;; 
;; Presumably, there may be something in how DEFOP is evaluted, in each
;; respective implementation.
;;
;; Specifically, the following forms do not evaluate successfully,
;; in those versions of SBCL:
;;
;; (%+ 1)
;;  --> wrong number of arguments: 2 (???)
;;
;; (@+ 1 1)
;;  --> The value (1 1) is not of type FIXNUM (????)
;;
;; Furthermore, the following form causes those versions of SBCL to
;; exhaust the control stack -- existing, on win32
;;
;;  (@+@ 1 2 3)
;;
;; siimlarly, though the following form should immediately result in
;; an error being signaled, but it results in the control stack being
;; exhausted, in SBCL:
;;
;; (@+@)
;;
;; Those forms have all been tested successfully in CCL.
;;
;;
;; It may have something to do with the method dispatching protocol in
;; SBCL's fork of PCL
;;
;; One notes the prevalence of SB-PCL::GF-DISPATCH calls in the backtrace
;; from the following:
;; (funcall (method-function (car (compute-applicable-methods-using-classes #'math::@+@ nil) )) 1 2 3)


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

  (let* ((monadic-op-name (intern (format nil "%~A" op)))
         #+SBCL (src (sb-c:source-location))
         (gf-monadic
          (ensure-generic-function monadic-op-name
                                   :lambda-list '(a)
                                   :argument-precedence-order '(a)
                                   #+SBCL :definition-source #+SBCL src
                                   ))
         (diadic-op-name (intern (format nil "@~A" op)))
         (gf-diadic 
          (ensure-generic-function diadic-op-name
                                   :lambda-list '(a b)
                                   :argument-precedence-order '(a b)
                                   #+SBCL :definition-source #+SBCL src
                                   )))
        
    (labels ((mklambda  (ll op specializers form)
               `(lambda ,ll 
                  (declare (inline ,op)
                           ,@(when specializers
                                   (mapcar #'(lambda (arg c)
                                               `(type ,(class-name c) ,arg))
                                           ll specializers)))
                  ,form))
             (makem (gf op specializers lambda form)
               (let* ((lform (mklambda lambda op specializers form))
                      (m
                       (progn 
                         #+NIL (warn "FOO ~S" lform)
                         (make-instance
                          'standard-method 
                          :specializers specializers
                          :lambda-list lambda
                          #+SBCL :definition-source #+SBCL src
                          :function (compile nil lform)))))
                 (add-method gf m))))
    
      (dolist (c classes)
        (let* ((%c (compute-class c)))
          ;; Define methods for diadic and monadic functions
          ;; specialized onto each C
          (makem gf-diadic op (list %c %c) '(a b)
                 `(funcall ,(fdefinition op) a b))
          (makem gf-monadic op (list %c) '(a)
                 `(funcall ,(fdefinition op) a))))
      
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
                 `(funcall ,(fdefinition op) a b))))

      (cond
        (variadic-p
         ;; Define a variadic function.
         ;;
         ;; FIXME: This produces a generic function that canot be
         ;; any further specialized
         (let* ((variadic-op-name (intern (format nil "@~A@" op)))
                (gf-variadic 
                 (ensure-generic-function variadic-op-name
                                          :lambda-list '(&rest values)
                                          #+SBCL :definition-source #+SBCL src
                                          )))
           (makem gf-variadic op nil '(&rest values)
                  `(progn
                    (unless (consp values)
                      (simple-program-error 
                       `,(format* "~A called with no arguments"
                                  (quote ,variadic-op-name))))
                    (let ((rest (cdr values)))
                      (cond
                        (rest 
                         (funcall ,gf-diadic 
                                  (car values)
                                  (apply ,gf-variadic rest)))
                        (t (funcall ,gf-monadic (car values)))))))

           (values gf-diadic gf-monadic gf-variadic)))
        (t (values gf-diadic gf-monadic nil))))))

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
(defgeneric @max@ (&rest values)
  (:method (&rest values)
    (let ((top (car values))
          (stack (cdr values)))
      (cond
        (stack
         (let* ((stack-max (apply #'@max stack))
                (top-gt-p (@> top stack-max)))
           (if top-gt-p top stack-max)))
        (t top)))))

;; (%max 1)
;; => 1
;; (@max 2 1)
;; => 2
;; (@max@ 1 3 2)
;; => 3

;; redefine @min@ to apply @< internally
(defgeneric @min@ (&rest values)
  (:method (&rest values)
    (let ((top (car values))
          (stack (cdr values)))
      (cond
        (stack
         (let* ((stack-min (apply #'@min stack))
                (top-lt-p (@< top stack-min)))
           (if top-lt-p top stack-min)))
        (t top)))))

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


