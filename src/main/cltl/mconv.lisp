;; mconv.lisp - procedures for measurement conversion

(in-package #:math)

(defgeneric convert-measurement (measurement to-type)
  (:method ((measurement measurement) (to-type symbol))
    (convert-measurement measurement (find-measurement-class to-type)))
  (:method ((measurement measurement) (to-type measurement-class))
    (let* ((src-type (class-of measurement))
	   (src-domain (measurement-domain src-type))
	   (dst-domain (measurement-domain to-type)))
      (unless (eq src-domain dst-domain)
	;; FIXME: Hypothetically "Could," in certain instances, but it
	;; would require some relatively complex factoring, as
	;; depening on the availability of registered, cross-domain
	;; conversion formulas.
	(simple-program-error "Cannot convert measurement of type ~S to type ~S"
			      src-type to-type))

      (let ((cf (find-conversion-factor src-type to-type dst-domain)))
	(make-measurement (* (measurement-magnitude measurement)
			     (factor-magnitude cf))
			  to-type
			  (+ (measurement-degree measurement)
			     (factor-exponent cf )))))))

;; (scalar-magnitude (convert-measurement (make-measurement 1 :|ft|) :|m|))
;; => 1200/3937 (m)
;; (scalar-magnitude (convert-measurement (make-measurement 1 :|m|) :|ft|))
;; => 3937/1200 (ft)

;; (scalar-magnitude (convert-measurement (make-measurement 1 :|m| 3) :|mi|))
;; => 3937/6336 (mi)

