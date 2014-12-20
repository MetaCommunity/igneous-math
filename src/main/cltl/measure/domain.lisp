;; domain-lisp - measurement domains

(in-package #:math)

(defgeneric measurement-domain-base-measure (instance))
(defgeneric measurement-domain-symbol (instance))


(defclass measurement-domain (pretty-printable-object
			      standard-class)
  ;; FIXME: #I18N slot definitions - PRINT-NAME/PRINT-LABEL
  ((base-measure
    :initarg :base-measure
    :type class
    :reader measurement-domain-base-measure)
   (symbol
    :initarg :symbol
    :type symbol
    :reader measurement-domain-symbol)
   (cf-lock
    :initform (make-lock "CONVERSION-FACTORS")
    :reader measurement-domain-cf-lock)
   (conversion-factors
    ;; Table of conversion factors for within the same measurement domain
    :initarg :conversion-factors
    :type vector
    :initform (make-array 0 :fill-pointer 0)
    :reader measurement-domain-conversion-factors)))

(validate-class measurement-domain)


(defmethod shared-initialize :around ((instance measurement-domain)
				      slots &rest initargs
				      &key base-measure)
  (let (args-changed-p)
    (when (and base-measure (symbolp base-measure))
      (let ((c (or (find-class base-measure nil) 
		   (ensure-forward-referenced-class base-measure))))
	(setf (getf initargs :base-measure) c)
	(setq args-changed-p t)))
    (cond
      (args-changed-p
       (apply #'call-next-method instance slots initargs))
      (t (call-next-method)))))
	


(define-condition measurement-domain-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     ;; FIXME: #I18N
     (format s "No measurement domain registered for name ~S"
             (entity-condition-name c)))))
 

(define-condition measurement-domain-redefinition (redefinition-condition)
  ()
  (:report 
   (lambda (c s)
     ;; FIXME: #I18N
     (let* ((p (redefinition-condition-previous-object c))
	    (n (redefinition-condition-new-object c))
	    (sym (measurement-domain-symbol p)))
     (format s "~<Redefining measurement domain ~S~> ~<(previous: ~S)~> ~<(new: ~S)~>"
	     sym n p)))))


(declaim (type (vector measurement-domain) %domains% ))

(defvar %domains% (make-array 7 :fill-pointer 0 
			      :element-type 'measurement-domain)
  "Internal storage for measurement domains.

This variable should be accessed with `%DOMAINS-LOCK%' held")


(defvar %domains-lock% (make-lock "%DOMAINS%")
  "Mutex lock for accessing `%DOMAINS%'")



(defun find-measurement-domain (name)
  ;; FIXME: #I18N query
  (declare (type (or symbol string) name)
	   ;; unable to check return type
	   #+NIL (values measurement-domain))
  (let ((s (etypecase name
	     (symbol name)
	     (string (intern* (read-from-string name)
			      (find-package '#:keyword))))))
    (with-lock-held (%domains-lock%)
      (or (find s %domains%
		:test #'eq
		:key #'measurement-domain-symbol)
	  (error 'measurement-domain-not-found
		 :name name)))))

(defun register-measurement-domain (domain)
  (declare (type class-designator domain)
           ;; "Type assertion too complex to check" (SBCL)
           #+NIL (values measurement-domain))
  (with-lock-held (%domains-lock%)
    (let* ((c (compute-class domain))
           (s (measurement-domain-symbol c))
	   (n (position s %domains%
			:test #'eq
			:key #'measurement-domain-symbol)))
      (declare (type measurement-domain c))
      (cond
	(n (warn 'measurement-domain-redefinition
		 :previous (aref %domains% n)
		 :new c)
	   (setf (aref %domains% n) c))
	(t (vector-push-extend c %domains%)))
      (values c))))

(defun enumerate-measurement-domains ()
  (coerce %domains% 'list))

;; (find-measurement-domain :electrical-current)

;; (mapcar #'measurement-domain-symbol (enumerate-measurement-domains))



;;; % Prototype: Geometric Measurement Domain

#+GEOM-MEASURE
(defgeneric measurement-domain-base-domain (domain)
  (:method ((domain measurement-domain))
    (values domain)))

#+GEOM-MEASURE
(defgeneric measurement-domain-degree (domain)
  (:method ((domain measurement-domain))
    (values 1)))

#+GEOM-MEASURE
(defclass geometric-measurement-domain (measurement-domain)
  ((base-domain
    :intiarg :base-domain
    :type measurement-class
    :reader measurement-domain-base-domain)
   (degree
    :initarg :degree
    :type fixnum
    :reader measurement-domain-degree)))
   
#+GEOM-MEASURE
(defvar %domain-table%
  (make-hash-table :test 'eq))


#+GEOM-MEASURE
(defun ensure-domain-cache (n)
  (declare (type n fixnum))
  (with-lock-held (%domains-lock%)
    (let ((it (gethash n %domain-table%)))
      (cond
	(it (values it t))
	(t (setq it (make-array 1 :fill-pointer 0))
	   (setf (gethash n %domains-table%) it)
	   (values it nil))))))


#+GEOM-MEASURE  ;; REDEF (PROTOTYPE)
(defun find-measurement-domain (name &optional (degree 1))
  ;; FIXME: #I18N query
  (declare (type (or symbol string) name)
	   (type fixnum degree)
	   ;; unable to check return type
	   #+NIL (values measurement-domain))
  (labels ((err ()
	     (error 'measurement-domain-not-found
		    :the-additional-arg degree
		    :name name)))
    (multiple-value-bind (cache existed-p)
	(ensure-domain-cache degree)
      (unless existed-p (err))
      (let ((s (etypecase name
		 (symbol name)
		 (string (intern* (read-from-string name)
				  (find-package '#:keyword))))))
	(with-lock-held (%domains-lock%)
	  (or (find s storage
		    :test #'eq 
		    :key #'measurement-domain-symbol)
	      ))))))

#+GEOM-MEASURE  ;; REDEF (PROTOTYPE)
(defun register-measurement-domain (domain)
  (declare (type measurement-domain domain)
	   ;; unable to check return type
	   #+NIL (values measurement-domain))
  (with-lock-held (%domains-lock%)
    (let* ((s (measurement-domain-symbol domain))
	   (storage (ensure-domain-cache (measurement-domain-degree domain)))
	   (n (position s storage
			:test #'eq
			:key #'measurement-domain-symbol)))
      (cond
	(n (warn 'measurement-domain-redefinition
		 :previous (aref %domains% n)
		 :new domain)
	   (setf (aref %domains% n) domain))
	(t (vector-push-extend domain %domains%)))
      (values domain))))


#+GEOM-MEASURE
(defun make-geometric-measurement-type (base degree)
  ;; cf. prototypes in unit-expr-proto.lisp
  ;; esp. DIADIC-MEASUREMENT-TYPE-QUERY
  
  )


#+GEOM-MEASURE
(defclass area (measurement-class)
  ;; prototypical geometric-measurement-domain
  ()
  (:metaclass geometric-measurement-domain)
  (:degree . 2)
  (:base-domain . length)
  (:print-name . "area")
  (:print-label . "area")
  (:symbol . :area)
  (:base-measure . square-meter))




;; #+GEOM-MEASURE-2 - towards combining measurement units of differing domains

#+GEOM-MEASURE-2
(defclass velocity (measurement-class)
  ;; m/s => m s^-1
  ()
  (:metaclass geometric-measurement-domain)
  (:degree . ???)
  (:base-domain . ???)
  (:print-name . "velocity") ;; ???
  (:print-label . "velocity") ;; ???
  (:symbol . :velocity) ;; ???
  (:base-measure . ???))

#+GEOM-MEASURE-2
(defclass acceleration (measurement-class)
  ;; m/s^2 => m s^-2
  ()
  (:metaclass geometric-measurement-domain)
  (:degree . ??)
  (:base-domain . ???)
  (:print-name . "acceleration") ;; ???
  (:print-label . "acceleration") ;; ???
  (:symbol . :acceleration) ;; ???
  (:base-measure . ???))
