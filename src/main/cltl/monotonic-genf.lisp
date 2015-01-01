;; monotonic-genf.lisp - definition of a MONOTONIC-GENERIC-FUNCTION

(in-package #:mcicl.math)

;; FIXME: Move code to mci-cltl-utils system

;; FIXME: This does not work in CCL
;;
;; Tested with (DEFGENERIC @GEOMETRIC-SUM ...)
;; followed by (geometric-sum 3 4)
;; using native MOP forms (C2MOP)

;;; % MOP Util

(defgeneric compute-method-lambda (genf lambda enviornment)
  (:documentation "Utility function for MAKE-METHOD-LAMBDA

See also:
* Initialization of Generic Function and Metahod Metaobjects.
  (The Common Lisp Object System MetaObject Protocol)
  Available: <http://www.alu.org/mop/concepts.html#init-generic>")
  (:method ((genf standard-generic-function) lambda environment)
    "Call MAKE-METHOD-LAMBDA on GENF, the class prototype of the
generic function method class of GENF, LAMBDA, and ENVIRONMENT"
    (make-method-lambda genf 
			(class-prototype (generic-function-method-class genf))
			lambda environment)))

(defmacro class-prototype* (class)
  (with-gensym (c)
    `(let ((,c ,class))
       (class-prototype 
	(etypecase ,c
	  (symbol (find-class ,c))
	  (class ,c))))))

#+NIL ;; unused
(defgeneric method-specifier (method)
  (:documentation
   "create a specifier for a method, such that would be compatible with MAKE-LOAD-FORM")
  (:method ((method standard-method))
    (list* (generic-function-name (method-generic-function genf))
	   (mapcar #'(lambda (a c)
		       (typecase c
			 (class (list a (class-name c)))
			 (t (list a c))))
		   (method-lambda-list method)
		   (method-specializers method))
	   (method-qualifiers method))))

;; (method-specifier (car (generic-function-methods #'@+)))



;;; %% Monotonic Generic Functions, Methods, and Method Combination

;; A Monotonic Generic Functions' effective method, when provided
;; with a set of arguments for which an applicable method is
;; avaialble, will call exactly one of the methods specialized on the
;; generic function -- namely, the most specific method.
;;
;; As such, the functions CALL-NEXT-METHOD and NEXT-METHOD-P wll not
;; be appliable within the METHOD-LAMBDA of a MONOTONIC-METHOD. 

(defclass monotonic-method-combination (method-combination)
  ())

#+SBCL ;; FIXME: portability
(defmethod method-combination-type-name  ((cmbn monotonic-method-combination))
  ;; e.g. for (DESCRIBE #'@+)
  (class-name (class-of cmbn)))

(defgeneric method-lambda-body (method))
(defgeneric (setf method-lambda-body) (new-value method))

(defclass monotonic-method (standard-method)
  ((lambda-body
    :initarg :lambda-body
    :type list
    :accessor method-lambda-body)))


(defclass monotonic-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class)
  (:default-initargs 
      :method-combination (class-prototype* 'monotonic-method-combination)
    :method-class (find-class 'monotonic-method)))


(defmethod compute-applicable-methods ((gf monotonic-generic-function)
				       args)
  (declare (ignore args))
  (let ((std-set (call-next-method)))
    (when std-set
      (list (car std-set)))))

(defmethod make-method-lambda ((genf monotonic-generic-function)
			       (method monotonic-method)
			       lambda environment)
  (declare (ignore genf environment))
  (with-gensym (method-lambda-args next-methods this-method)
    ;; referencing the MOP specification, CLOSER-MOP, and SB-PCL
    (setf (method-lambda-body method) lambda)
    `(lambda (,method-lambda-args ,next-methods ,this-method)
       (declare (ignore ,next-methods))
       (labels ((call-next-method (&rest cnm-args)
		  (apply #'no-next-method 
			 (method-generic-function ,this-method)
			 ,this-method cnm-args))
		(this-method () (values ,this-method))
		(next-method-p () nil))
	 (declare (ignorable (function call-next-method)
			     (function this-method)
			     (function next-method-p)))
	 ;; KLUDGE. A direct reference to the compiled form of LAMBDA
	 ;; most likely cannot be dumped to a FASL file.
	 ;;
	 ;; Note that the COMPILE call should only be called once,
	 ;; however, when the METHOD-LAMBDA itself is compiled.
	 ;;
	 ;; FIXME: Capture errors and warnings from the COMPILE call
	 (apply (compile* nil ,lambda)
		,method-lambda-args)))))

(defmethod compute-effective-method ((genf monotonic-generic-function)
				     (combin monotonic-method-combination)
				     applicable-methods)
  (declare (ignore genf combin))
  (let ((the-method (car applicable-methods)))
    (values `(call-method ,the-method nil ,the-method)
	    nil)))

#+NIL
(defgeneric frob (a b)
  (:generic-function-class monotonic-generic-function)
  (:method ((a number) (b number))
    (complex a b))
  (:method ((a fixnum) (b fixnum))
    (+ a b)))

;; (frob 1 2)
;; => 3

;; (frob 1.0 2.0)
;; => #C(1.0 2.0)

;; (frob 1.0d0 2.0d0)
;; => #C(1.0d0 2.0d0)

;; (frob 1 2.0)
;; => #C(1.0 2.0)
