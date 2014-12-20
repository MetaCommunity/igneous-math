;; dimensionless.lisp - standard dimensionless measurements

(in-package #:math)

;;; %% Angular Measure

(defclass plane-angle (measurement-class)
  ()
  (:metaclass measurement-domain)
  ;; FIXME: #I18N
  (:print-name . "angular measure")
  (:print-label . "angular measure")
  (:symbol . :plane-angle)
  (:base-measure . radian))

(register-measurement-domain (find-class 'plane-angle))

(defclass radian (measurement)
  ;; FIXME: Define as COMPOUND-MEASUREMENT-CLASS with units m m^-1
  ;; FIXME: Define corresponding STERADIAN class in measurement domain
  ;; SOLID-ANGLE, units m^2 m^-2
  ()
  (:metaclass plane-angle)
  (:print-name . "radian")
  (:print-label . "rad")
  (:symbol . :|rad|))

(register-measurement-class (find-class 'radian))

;; (object-print-name (measurement-domain-base-measure (find-class 'plane-angle)))
;; => "radian"

(defclass degree (measurement)
  ()
  (:metaclass plane-angle)
  (:base-factor . #.(/ (rational pi) 180) )
  (:print-name . "degree")
  (:print-label . "deg")
  (:symbol . :|deg|))

(register-measurement-class (find-class 'degree))

;; (scalar-magnitude (convert-measurement (make-measurement (rational pi) :|rad|) :|deg|))
;; => 180

;;; %% Dimensionless Measure

(defclass dimensionless-measure (measurement-class)
  ;; FIXME: Consider undefining this class. It should be possible to
  ;; define any effectively dimensionless measure (e.g. for index of
  ;; refraction, per "Snell's Law") in terms of base units from which
  ;; it may be derived (e.g m s^-2 m^-1 s^2)
  ()
  (:documentation "Informal domain for dimensionless measures")
  (:metaclass measurement-domain)
  ;; FIXME: #I18N
  (:print-name . "dimensionless measure")
  (:print-label . "dimensionless measure")
  (:symbol . :dimensionless-measure)
  (:base-measure . unity))

(register-measurement-domain (find-class 'dimensionless-measure))

(defclass unity (measurement)
  ()
  (:documentation "Informal base measure of dimensionless measures")
  (:metaclass dimensionless-measure)
  ;; FIXME: Specialize print-object so as to not print the measurement
  ;; label for any instance of a DIMENSIONLESS-MEASURE class,
  ;; excepting those defined with conventional masurement labels
  (:print-name . "unity")
  (:print-label . "u")
  #+PROTOTYPE-M (:disable-print-label t)
  (:symbol . :|u|))

(register-measurement-class (find-class 'unity))
