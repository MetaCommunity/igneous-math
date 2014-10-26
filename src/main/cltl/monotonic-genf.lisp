;; monotonic-genf.lisp - definition of a MONOTONIC-GENERIC-FUNCTION

(in-package #:math)

;; FIXME: Move code to mci-cltl-utils system

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


(defclass monotonic-method (standard-method)
  ())

(defclass monotonic-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class)
  (:default-initargs 
      :method-combination (class-prototype* 'monotonic-method-combination)
    :method-class (find-class 'monotonic-method)))


(defmethod compute-applicable-methods ((gf monotonic-generic-function)
				       args)
  (let ((std-set (call-next-method)))
    (when std-set
      (list (car std-set)))))

(defmethod make-method-lambda ((genf monotonic-generic-function)
			       (method monotonic-method)
			       lambda environment)
  (declare (ignore environment))
  (with-gensym (method-lambda-args next-methods)
    ;; referencing AMOP, CLOSER-MOP
    `(lambda (,method-lambda-args ,next-methods)
       (declare (ignore ,next-methods))
       (labels ((call-next-method (&rest cnm-args)
		  (apply #'no-next-method ,genf ,method cnm-args))
		(next-method-p () nil))
	 (apply ,(compile nil lambda)
		,method-lambda-args)))))

(defmethod compute-effective-method ((genf monotonic-generic-function)
				     (combin monotonic-method-combination)
				     applicable-methods)
  (declare (ignore genf combin))
  (values `(call-method ,(car applicable-methods))
	  nil))

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
