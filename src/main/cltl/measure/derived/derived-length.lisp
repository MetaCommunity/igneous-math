;; length-measure.lisp - derived measurements of length

(in-package #:math)

;;; %% Derived Measures of Length

;; referencing [NIST SP811]

(defclass us-survey-foot (measurement)
  ;; prototypical, derived measurement unit
  ;;
  ;; FIXME: Denote this as a "US Survey Foot" [1893], per NIST SP811
  ;; section B.6 
  ;;
  ;; Also define:
  ;;  * US Survey Mile [1893] (5280 US Survey ft)
  ;;  * International foot [1959] 0.3048 m
  ;;  * International mile  [1959] 5280 US internationa foot
 ()
  (:metaclass length)
  (:print-name . "foot [1893]")
  (:print-label . "ft [1893]")
  (:base-factor . 1200/3937)
  (:symbol . :|ft_1893|)
  (:documentation "US Survey Foot [1893]")
  #+OBJECT-ANNOTATION
  ;; ^ towards a concept similar to OWL annotation properites
  ;; in an application for annotation of Common Lisp objects,
  ;; ...towards something perhaps somewhat more specialized than
  ;; symbol plists, and onto "The Semantic Web" etc
  ;;
  ;; Here, just a small usage example, for denoting an exact "source 
  ;; resource" from which a measurement conversion ratio has been
  ;; derived, manually, by the author:
  (:annotation
   ;; #i<IRI> reader macro (TBD)
   (:defined_by #i<http://physics.nist.gov/pubs/sp811/> B.6 42)
   ;; ^ specifically SP811 section B.6 (p. 42)
   ))

(register-measurement-class (find-class 'us-survey-foot))


;; (base-magnitude (make-measurement 1 :|ft_1893|))
;; => 1200/3937 i.e meters (FIXME: Not returning that value, presently)

;; (base-convert-measurement (make-measurement 1 :|ft_1893|))
;; => #<METER 1200/3937 m {1012630AC3}>

;;
;; (float 1200/3937 pi)
;; => 0.3048006096012192d0 
;;  ^ 1 foot => "this many" meters (double float precision)
;;  ^ approximately 0.30408 as per SP811 (less than single-float precision)

;; (find-conversion-factor :|ft_1893| :|m| (find-class 'length))
;; (find-conversion-factor :|m| :|ft_1893| (find-class 'length))

;; (make-measurement 1 :|ft_1893| 3)
;; ^ the illustrious kft
;; (make-measurement 1 :|ft_1893| -3)
;; ^ correspondingly, the larch millifoot


(defclass us-survey-mile (measurement)
  ()
  ;; FIXME: Denote as US Survey Mile [1893] - See previous
  (:metaclass length)
  (:print-name . "mile [1893]")
  (:print-label . "mi [1893]")
  (:base-factor . #.(* 5280 1200/3937))
  (:symbol . :|mi_1893|)
  (:documentation "US Survey Mile [1893]")
  #+OBJECT-ANNOTATION
  (:annotation
   (:defined_by #i<http://physics.nist.gov/pubs/sp811/>)
   ))

(register-measurement-class (find-class 'us-survey-mile))
