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
    :reader measurement-domain-symbol)))

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
