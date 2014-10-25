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

(defconstant %numeric-instance-classes%
    (let ((the-end-classes
           (compute-end-classes (find-class 'number))))
      #+CCL
      (cons (find-class 'fixnum) the-end-classes)
      #-CCL
      the-end-classes))


;;; % Overloading for Comutative Functions

(defun defop (op &optional (classes %numeric-instance-classes%))
  (declare (type symbol op)
           (type cons classes)
           (values generic-function
                   generic-function
                   generic-function))
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
                                   ))
         (variadic-op-name (intern (format nil "@~A@" op)))
         (gf-variadic 
          (ensure-generic-function variadic-op-name
                                   :lambda-list '(&rest values)
                                   #+SBCL :definition-source #+SBCL src
                                   )))
    
    (macrolet ((makem ((gf op &rest specializers) lambda &body body )
                 (with-gensym (m)
                   `(let ((,m
                           (make-instance
                            'standard-method 
                            :specializers (list ,@specializers)
                            :lambda-list (quote ,lambda)
                            #+SBCL :definition-source #+SBCL src
                            :function  
                            (lambda ,lambda
                              (declare
                               (inline ,op)
                               ,@(when specializers
                                       (mapcar #'(lambda (a c)
                                                   `(type ,c ,a))
                                               lambda 
                                               specializers)))
                              ,@body))))
                      (add-method ,gf ,m)))))

      (dolist (c classes)
        (let* ((%c (compute-class c)))
          (makem (gf-diadic op %c %c) (a b)
                 (funcall (fdefinition op) a b))
          (makem (gf-monadic op %c) (a)
                 (funcall (fdefinition op) a))))

      (makem (gf-variadic op)
             (&rest values)
             (progn
               (unless (consp values)
                 (simple-program-error 
                  `,(format* "~A called with no arguments"
                             variadic-op-name)))
                (let ((rest (cdr values)))
                (cond
                  (rest 
                   (funcall gf-diadic 
                            (car values)
                            (apply gf-variadic rest)))
                  (t (funcall gf-monadic (car values)))))))


      (values gf-diadic gf-monadic gf-variadic))))

;; define functions:
;; %+ @+ @+@
;; %- @- @-@
;; %* @* @*@
;; %/ @/ @/@
;; with respective monadic, diadic (same class), and variadic methods

(defop '+)
(defop '-)
(defop '*)
(defop '/)

;; single instane test, ensure +
;; (%+ 1)
;; => 1
;; single instane test, ensure +
;; (@+ 1 1)
;; => 2

;; single instane test, ensure +
;; (@+@ 1 2 3)
;; => 6

;; FIXME : run unit tests on each operation, for each %NUMERIC-END-CLASSES%


;; single instance test, ensure error
;; (@+@)

(defop '=)
(defop '/=)
(defop '<)
(defop '>)
(defop '<=)
(defop '>=)

(defop 'max)
(defop 'min)

;; redefine @max@
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


;; redefine @min@
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



;; NB: The following methods effectively loose compiler optimization,
;; such that may be available when A and B are delcared explicitly for
;; their respective numeric types -- namely, in instances when the
;; classes of A and B are not EQ.
;;
;; Though it lacks compiler optimization, in thise forms, this
;; approach nonetheless allows for overloading of these mathematical
;; operations.

(defmethod @+ ((a number) (b number))
  (+ a b))
(defmethod @- ((a number) (b number))
  (- a b))
(defmethod @* ((a number) (b number))
  (* a b))
(defmethod @/ ((a number) (b number))
  (/ a b))

;;; % Overloading for Diadic Non-Comutative Functions

(defgeneric @expt (a b)
  (:method ((a number) (b number))
    (expt a b)))



;;; % Overloading for Transcendental Functions


;;; % Overloading for Alternately Monadic/Diadic Functions

;; FIXME: Possibly loosing more optimizations, here.
;; When possible, try to call directly to the underlying machine
;; instructions (Architecture-specific code - cf. SSE, etc)

(defgeneric %floor (a)
  (:method ((a number))
    (floor a)))

(defgeneric %floor (a b)
  (:method ((a number) (b number))
    (floor a b)))



;;; % Overloading for Other Monadic Functions

;; FIXME : Observing the implementation of the respetive CL functions
;; in CCL -- e.g. cl:minusp -- it seems that some optimization is being
;; lost in the broad dispatching on NUMBER. Ideally, this
;; implementation would present all possible optimizations to the
;; compiler

(defmethod %minusp (a)
  (:method ((a number))
    (minusp a)))

(defmethod %plusp (a)
  (:method ((a number))
    (plusp a)))

(defmethod %zerop (a)
  (:method ((a number))
    (zerop a)))

