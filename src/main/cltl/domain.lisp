;; domain-lisp - measurement domains

(in-package #:math)

(defgeneric measurement-domain-base-measure (instance))
(defgeneric measurement-domain-symbol (instance))


(defclass measurement-domain (pretty-printable-object
			      standard-class)
  ;; FIXME: #I18N slot definitions - PRINT-NAME/PRINT-LABEL
  ((base-measure
    :initarg :base-measure
    :type measurement-class
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

(define-condition measurement-domain-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     ;; FIXME: #I18N
     (format s "No measurement domain registered for name ~S"
             (entity-not-found-name c)))))
 

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
	   (values measurement-domain))
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
  (declare (type measurement-domain domain)
	   (values measurement-domain))
  (with-lock-held (%domains-lock%)
    (let* ((s (measurement-domain-symbol domain))
	   (n (position s %domains%
			:test #'eq
			:key #'measurement-domain-symbol)))
      (cond
	(n (warn 'measurement-domain-redefinition
		 :previous (aref %domains% n)
		 :new domain)
	   (setf (aref %domains% n) domain))
	(t (vector-push-extend domain %domains%)))
      (values domain))))

(defun enumerate-measurement-domains ()
  (coerce %domains% 'list))

;; (find-measurement-domain :electrical-current)

;; (mapcar #'measurement-domain-symbol (enumerate-measurement-domains))


;;; % CONVERSION-FACTOR

(defclass* (conversion-factor
	    :conc-name #:factor-)
    ()
  ((source-unit
    measurement-class
    :read-only t)
   (magnitude 
    real
    :read-only t)
   (exponent 
    fixnum
    :initform 0
    :read-only t)
   (destination-unit
    measurement-class
    :read-only t)))

(defmethod print-object ((object conversion-factor) stream)
  (macrolet ((safely (form)
	       `(ignore-errors ,form)))
    (print-unreadable-object (object stream :type t)
      (format stream "(1 ~A) => (~A~@D ~A)"
	      (safely (object-print-label 
		       (factor-source-unit object)))
	      (safely (factor-magnitude object))
	      (or (safely (factor-exponent object))
		  0)
	      (safely (object-print-label
		       (factor-destination-unit object)))))))

(defun make-conversion-factor (source-unit dest-unit
			       factor-magnitude 
			       &optional (factor-exponent 0))
  (declare (type measurement-class source-unit dest-unit)
	   (type real factor-magnitude)
	   (type fixnum factor-exponent))
  (values (make-instance 'conversion-factor
			 :source-unit source-unit
			 :destination-unit dest-unit
			 :magnitude factor-magnitude
			 :exponent 0)))

(defgeneric find-conversion-factor (source-unit dest-unit domain)
  (:method ((source-unit symbol) (dest-unit symbol)
	    (domain measurement-domain))
    (find-conversion-factor (find-measurement-class source-unit)
			    (find-measurement-class dest-unit)
			    domain))
  (:method ((source-unit measurement-class) (dest-unit measurement-class)
	    (domain measurement-domain))
    (with-lock-held ((measurement-domain-cf-lock domain))
      (let ((factors (measurement-domain-conversion-factors domain)))
	;; This does not completely walk the measurement-domains table,
	;; performing only a cursory, one-off search towards DST.
	;;
	;; If each derived unit, within a single measurement domain,
	;; registers a factor for converting that unit to the domain's
	;; base measure, and the reciptrocal conversion is also
	;; registered, then it would be possible to convert  
	;; between any registered units within a measurement domain,
	;; using only a table of factors
	(or (find source-unit factors
		  :test #'(lambda (u cf)
			    (and (eq source-unit (factor-source-unit cf))
				 (eq dest-unit (factor-destination-unit cf)))))
	    (simple-program-error
	     "No conversion factor available for converting ~S to ~S within ~S"
	     src dst domain))))))

(defgeneric register-measurement-conversion-factor (factor domain)
  (:method ((factor conversion-factor) 
	    (domain measurement-domain))
    (with-lock-held ((measurement-domain-cf-lock domain))
      ;; FIXME: This vector storage model may not be very efficient, 
      ;; onto a "two part" key, contrasted to an EQUALP hash table.
      (let* ((table (measurement-domain-conversion-factors domain))
	     (n  (position factor table
			   :test (lambda (f-in f-test)
				   (and (eq (factor-source-unit f-in)
					    (factor-source-unit f-test))
					(eq (factor-destination-unit f-in)
					    (factor-destination-unit f-test)))))))
	(cond
	  (n (setf (aref table n) factor))
	  (t (vector-push-extend factor table)))))))


