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

(defgeneric domain-of (instance)
  (:method ((instance measurement-domain))
    (values instance)))

(defmethod shared-initialize :around ((instance measurement-domain)
				      slots &rest initargs
				      &key (base-measure nil bmp)
                                        &allow-other-keys)
  (let (args-changed-p)
    (when bmp
      (let ((c (or (find-class base-measure nil) 
		   (ensure-forward-referenced-class base-measure))))
	(setf (getf initargs :base-measure) c)
	(setq args-changed-p t)))
    (cond
      (args-changed-p
       (apply #'call-next-method instance slots initargs))
      (t (call-next-method)))))



(defmethod finalize-inheritance :after ((class measurement-domain))
  (labels ((find-slot (sl where)
             (find sl (class-slots where)
                   :key #'slot-definition-name
                   :test #'eq))
           (find-super-value (slot sought-type
                                   &optional 
                                   (supers  
                                    (cdr (class-precedence-list class))))
             (dolist (c supers (values nil nil))
               (when (and (typep c sought-type)
                          (find-slot slot c)
                          (slot-boundp c slot))
                 (warn "WOOHOO! Found ~A in ~A for ~A"
                       slot c class)
                 (return (values (slot-value c slot)
                                 t)))))
           (maybe-inherit-slot (slot sought-type)
             (when (not (slot-boundp class slot))
               (multiple-value-bind (super-v foundp) 
                   (find-super-value slot sought-type)
                 (when foundp
                   (setf (slot-value class slot)
                         super-v))))))
    (maybe-inherit-slot 'utils::print-label 'pretty-printable-object)
    (maybe-inherit-slot 'utils::print-name 'pretty-printable-object)
    ;; FIXME: WHY is BASE-MEASURE still not being inherited?
    (maybe-inherit-slot 'base-measure 'measurement-domain)
    #+NO (maybe-inherit-slot 'symbol 'measurement-domain) ;; KLUDGE
    )
  (unless (documentation class 'type)
    (handler-case
        (with-accessors ((print-name object-print-name)) class
          (setf (documentation class 'type)
                (simplify-string
                 ;; FIXME: Note Base / Derived measurement domain here... 
                 ;; (KLUDGE)
                 (format nil "Measurement domain for quantities of ~A" 
                         print-name))))
      (unbound-slot (c)
        (simple-style-warning  "~<Unable to set documentation for ~S~> ~<(~A)~>"
                               class c)))))

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
