;; mconv.lisp - procedures for measurement conversion

(in-package #:math)

(define-condition conversion-domains-mismatch (error)
  ((source-domain
    :initarg :source-domain
    :reader condition-source-domain)
   (dest-domain
    :initarg :destination-domain
    :reader condition-destination-domain))
  (:report
   (lambda (c s)
     (format s "Domains Mismatch - Cannot convert measument of domain ~A to domain ~A"
             (condition-source-domain c)
             (condition-destination-domain c)))))

(defun verify-conversion-domain (src-type dst-type)
  (let ((src-domain (domain-of src-type))
        (dst-domain (domain-of dst-type)))
    (cond
      ((eq src-domain dst-domain)
       (values dst-domain))
      (t
       (error 'conversion-domain-mismatch
              :source-domain src-domain
              :destination-domain dst-domain)))))


(defgeneric convert-measurement (measurement to-type)
  ;; FIXME: Misaligned with `BASE-MAGNITUDE', `BASE-CONVERT-MEASUREMENT'
  (:method ((measurement measurement) (to-type symbol))
    (convert-measurement measurement (find-measurement-class to-type)))
  (:method ((measurement measurement) (to-type measurement-class))
    (let* ((src-type (class-of measurement))
           (domain (verify-conversion-domain src-type to-type))
           (base-type (measurement-domain-base-measure domain)))
      (cond
        ((eq src-type to-type)
         ;; FIXME: Should copy-object ?
         (values measurement))
        ((eq base-type to-type)
         ;; use BASE-MAGNITUDE as shortcut  (?)
         (make-measurement (base-magnitude measurement) base-type))
        (t ;; factoring
         (let ((cf (find-conversion-factor src-type to-type)))
           (make-measurement (* (measurement-magnitude measurement)
                                (factor-magnitude cf))
                             to-type
                             (+ (measurement-degree measurement)
                                (factor-exponent cf )))))))))

;; (scalar-magnitude (convert-measurement (make-measurement 1 :|ft_1893|) :|m|))
;; => 1200/3937 (m)
;; (scalar-magnitude (convert-measurement (make-measurement 1 :|m|) :|ft_1893|))
;; => 3937/1200 (ft)

;; (scalar-magnitude (convert-measurement (make-measurement 1 :|m| 3) :|mi_1893|))
;; => 3937/6336 (mi)

(defun base-convert-measurement (measurement)
  ;; FIXME : Misaligned with `BASE-MAGNITUDE'
  (declare (type measurement measurement)
	   (values measurement))
  (let ((base-mc (measurement-domain-base-measure 
		  (domain-of measurement))))
    (cond
      ((eq base-mc (class-of measurement)) 
       ;; ? ;; FIXME: TEST THIS
       (values measurement))
      (t (values (convert-measurement measurement base-mc))))))

;; (scalar-magnitude (base-convert-measurement (make-measurement 1 :|ft_1893|)))
;; => 1200/3937

;; (scalar-magnitude (base-convert-measurement (make-measurement 1 :|m|)))
;; => 1


(defun base-magnitude (m)  
  "Calculate the scalar magnitude of the measurement M for the base
measurement unit of M"
  ;; FIXME : Misaligned with `BASE-CONVERT-MEASUREMENT',
  ;; `CONVERT-MEASUREMENT', `SCALAR-MAGNITUDE'
  (declare (type measurement m)
           (values real))
  (let* ((unit (class-of m))
         ;; assuming same exponent base for both M and BASE - SHORTCUT
	 (expt-base (measurement-factor-base unit))
         
         (base-m (measurement-base-factor unit))
         (base-d (measurement-base-factor-exponent unit))

	 (m-m  (measurement-magnitude m))
         (m-d (measurement-degree m)))
    ;; Note that this effectively ignores the measurement conversion
    ;; model, and makes no reference to the domain of the MEASUREMENT
    ;;
    ;; Essentially, this function uses a "Short cut" for conversion to
    ;; base measurement, using the BASE-FACTOR and
    ;; BASE-FACTOR-EXPONENT of the UNIT for the measurement
    
    ;; FIXME - DESIGN AMBIGUITY => MISALIGNMENT
    ;; 
    ;; This function appears to return not the magnitude as in
    ;; {magnitude, degree} but rather the value of the magnitude
    ;; reduced per degrees in measurement conversion (i.e. source
    ;; degree => dest-degree)
    
    (* m-m (expt expt-base m-d)
       base-m (expt expt-base base-d))))

;; BEGIN: Example

;; (measurement-factor-base (find-class 'meter))
;; => 10
;;
;; (measurement-base-factor (find-class 'meter))
;; => 1
;;
;; (measurement-base-factor-exponent (find-class 'meter))
;; => 0

;; (measurement-factor-base (find-class 'gram))
;; => 10
;;
;; (measurement-base-factor (find-class 'gram))
;; => 1/1000
;;
;; (measurement-base-factor-exponent (find-class 'meter))
;; => 0

;; :END


;; (base-magnitude (make-measurement 1 :|m| 3))
;; => 1000

;; (base-magnitude (make-measurement 1 :|m| -3))
;; => 1/1000

;; (base-magnitude (make-measurement 1/5 :|m|))
;; => 1/5

;; (base-magnitude (make-measurement 1/5 :|m| -3))
;; => 1/5000


(defgeneric scalar-magnitude (scalar)
  ;; This function had its origins in the definition of the
  ;; measurement prefix model 

  ;; FIXME: AMBIGUITY/MISALIGNMENT ONTO:
  ;;  * `SCALAR-MAGNITUDE'
  ;;  * `BASE-MAGNITUDE'
  ;;  * `MESAUREMENT-MAGNITUDE'
  ;;
  ;; See also: README.md, specifically the discussion under the
  ;; heading, "Meaning and Application of 'Magnitude' and 'Value' in
  ;; Measurements"

  (:method ((scalar measurement))
    "Return the magnitude of the SCALAR measurement onto the base unit
for the measurement"
    (base-magnitude scalar)))

;; (scalar-magnitude (make-measurement 1 :|m| -3))
;; => 1/1000

