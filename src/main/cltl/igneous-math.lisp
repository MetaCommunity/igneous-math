
(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (dolist (s '(#:info.metacommunity.cltl.utils
               #:closer-mop
               #:bordeaux-threads))
    (asdf:operate 'asdf:load-op s))

  (defpackage #:info.metacommunity.cltl.math
    (:shadowing-import-from 
     ;; prefer the implementation's own forms to those defined in C2MOP
     #:cl
     #:defgeneric
     #:defmethod
     #:standard-generic-function)

    (:use #:info.metacommunity.cltl.utils
          #:bordeaux-threads
          #:c2mop
          #:cl))
  )


(in-package #:info.metacommunity.cltl.math)

;; sidebar: measurement definitions in hytime
;;
;; cf. 
;; http://www.cs.utexas.edu/users/novak/units95.html
;;
;; http://www.bipm.org/en/publications/si-brochure/metre.html
;;  -> http://www.bipm.org/en/publications/si-brochure/download.html
;;
;; http://www.is-thought.co.uk/schedule.htm
;; http://crism.maden.org/consulting/pub/hytime/meas.html
;; http://www.hytime.org/materials/hi2mdhyt.sgm
;; ^ cf %hygrand, "


;; see also:
;; http://physics.nist.gov/cuu/units/
;; http://physics.nist.gov/pubs/sp811/appenb.html 
;; ^ esp. for conversions regarding foot, mile, yard , ...


;; referencing http://www.bipm.org/en/publications/si-brochure/
;; and http://physics.nist.gov/pubs/sp811/contents.html
    

(defmacro defclass* (name-form (&rest superclasses) 
                             (&rest slot-definitions)
                     &rest initargs)
  (destructuring-bind (name &key (conc-name nil cnp))
      (cond
        ((symbolp name-form) (list name-form))
        (t name-form))

    (unless (or cnp conc-name)
      (setq conc-name (make-symbol (format nil "~A-" name))))

    (labels ((acc (slot read-only-p)
               (when conc-name
                 (list (if read-only-p :reader :accessor)
                       (intern-formatted "~A~A" conc-name slot))))
             (initarg (slot)
               (list :initarg (intern (symbol-name slot) :keyword))))
             
      `(defclass ,name (,@superclasses) 
         ,(mapcar (lambda (spec)
                    (destructuring-bind (name &optional (type t)
                                              &key read-only)
                        spec
                      `(,name ,@(acc name read-only)
                          ,@(initarg name)
                          :type ,type)))
                  slot-definitions)
         ,@initargs))))

#+NIL
(macroexpand-1 (quote
(defclass* (frob :conc-name #:a-frob-) ()
  ((b)
   (c real :read-only t)))
))

(defgeneric measurement-quantity-name (instance))
(defgeneric measurement-print-label (instance))
(defgeneric measurement-print-name (instance))
(defgeneric measurement-symbol (instance))

(defclass* (measurement-domain
            :conc-name #:measurement-)
    ()
  ((quantity-name simple-string :read-only t)
   (print-label simple-string :read-only t)
   (print-name simple-string :read-only t)
   (symbol symbol :read-only t)))


(defclass measurement-class (measurement-domain standard-class)
  ())


(define-condition class-nont-found (error)
  ((s
    :initarg :measurement-symbol
    :reader frob-measurement-symbol))
  (:report
   (lambda (c s)
     (format s "No measureent class registered for ~S"
             (frob-measurement-symbol c)))))

   

#+SBCL ;; FIXME: also port to other impmls. using PCL
(defmethod sb-mop:validate-superclass ((class measurement-class)
                                       (superclass standard-class))
  (values t))


(let ((%classes% (make-array 7 :fill-pointer 0))
      (%classes-lock% (make-lock "%CLASSES%")))
  (defun register-measurement-class (c)
    (with-lock-held (%classes-lock%)
      (let* ((s (measurement-symbol c))
             (n (position s %classes%
                          :test #'eq
                          :key #'measurement-symbol)))
        (cond 
          ((and n (not (eq (aref %classes% n) c)))
           (simple-style-warning 
            "Redfining measurement class for ~S" s)
           (setf (aref %classes% n) c))
          (t (vector-push-extend c %classes%)))
        (values c))))

  (defun find-measurement-class (s)
    (declare (type symbol s)
             (values measurement-class))
    (with-lock-held (%classes-lock%)
      (or (find s %classes%
               :test #'eq
               :key #'measurement-symbol)
          (error 'class-not-found :measurement-symbol s))))
  )
  


(defgeneric measurement-magnitude (instance))

(defclass* measurement ()
    ((magnitude real)))



;; FIXME: does not work in SBCL, but probably should.
;;
;; In macroexapansion for DEFBOUNCE%, NAME is said to be unbound
;;
;; something about the walker hopping over a binding??
;; - https://bugs.launchpad.net/sbcl/+bug/1368847
;;
;; so, must define the methods, each, manually
#+NIL
(labels ((defbounce (sl)
           (let ((name 
                  (intern-formatted "~A~A" 
                                    (quote #:measurement-)
                                    (slot-definition-name sl))))
             (macrolet ((defbounce% ()
                          `(defmethod ,name ((instance measurement))
                             (,name (class-of instance)))))
               (defbounce%)))))
  
  ;; for direct slots in MEASUREMENT-CLASS, define accessors onto
  ;; MEASUREMENT to access the respective slot of the same class
  (dolist (sl (class-direct-slots 
               (find-class 'measurement-domain)))
    (defbounce sl)))

(defmethod measurement-quantity-name ((instance measurement))
  (measurement-quantity-name (class-of instance)))

(defmethod measurement-print-label ((instance measurement))
  (measurement-print-label (class-of instance)))

(defmethod measurement-print-name ((instance measurement))
  (measurement-print-name (class-of instance)))

(defmethod measurement-symbol ((instance measurement))
  (measurement-symbol (class-of instance)))



(defmethod print-object ((object measurement) (stream stream))
  (print-unreadable-object (object stream :type t :identity t)
    (multiple-value-bind (mag boundp)
        (slot-value* object 'magnitude)
      (princ (cond
               (boundp mag)
               ;; FIXME: #I18N
               (t "{no magnitude}"))
             stream)
    (measurement-print-label (class-of object)))))

;;; define base unit classes
(labels ((do-def (c quantity print-label print-name name)
           (let ((c (c2mop:ensure-class 
                     c
                     :direct-superclasses (list (find-class 'measurement))
                     :symbol name
                     :print-name print-name
                     :print-label print-label
                     :quantity-name quantity
                     :metaclass (find-class 'measurement-class)
                     #+SBCL :definition-source 
                     #+SBCL (sb-c:source-location) 
                     )))
             (register-measurement-class c))))
  (mapcar (lambda (spec)
            (destructuring-bind 
                  (c quantity print-label print-name name) spec
              (do-def c quantity print-label print-name name)))
          '((meter "length" "metre" "m" :m)
            (kilogram "mass" "kilogram" "kg" :kg)
            (second "time, duration" "second" "s" :s)
            (ampere "electric current" "ampere" "A" :a)
            (kelvin "thermodyamic temperature" "kelvin" "K" :k)
            (mole "amount of substance" "mole" "mol" :mol)
            (candela "luminous intensity" "candela" "cd" :cd)))
  )

;; (find-measurement-class :m)
;; (find-measurement-class :kg)
;; (find-measurement-class :s)
;; (find-measurement-class :a)
;; (find-measurement-class :k)
;; (find-measurement-class :mol)
;; (find-measurement-class :cd)

(defun make-measurement (magnitude unit)
  (declare (type real magnitude)
           (type class-designator unit))
  (make-instance (find-measurement-class unit)
                 :magnitude magnitude))

;; (make-measurement 1 :m)

;; (measurement-print-name (make-measurement 1 :m))
;; => "m"


;; (defgeneric convert (scalar unit)
;;   (:method (scalar (unit symbol))
;;     (convert scalar (find-unit-using-symbol unit))
;;     ))
                   
  

;; SEE ALSO...
;; http://goldbook.iupac.org/list_math.html
;; esp. http://goldbook.iupac.org/list_goldbook_quantities_defs_A.html


;; referencing http://www.hytime.org/materials/hi2mdhyt.sgm
;;
;; notation names and public identifiers, for 'standard measurement unit'
;; (SMU) definitions...
;;
;; from the HyTime Granule Definition Notation module ...
;;
;; of "ISO/IEC 10744:1997", i.e. Hypermedia/Time-based Structuring
;; Language (HyTime)
;;
;; gQuantum "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"
;; SIsecond "ISO/IEC 10744:1997//NOTATION Systeme International second//EN"
;; SImeter "ISO/IEC 10744:1997//NOTATION Systeme International meter//EN"
;; virTime "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"
;; virSpace "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"
;; SIkg "ISO/IEC 10744:1997//NOTATION Systeme International kilogram//EN"
;; SIcd ""ISO/IEC 10744:1997//NOTATION Systeme International candela//EN"
;; SIampere "ISO/IEC 10744:1997//NOTATION Systeme International ampere//EN"
;; SImole "ISO/IEC 10744:1997//NOTATION Systeme International  mole//EN"
;; SIradian "ISO/IEC 10744:1997//NOTATION Systeme International radian//EN"
;; SIsr "ISO/IEC 10744:1997//NOTATION Systeme International steradian//EN"
;;
;; Issue: The gQuantum, virTime and virSpace notations share the same
;; public idenifier, but may be differentiated by their respective
;; notation names. Though practically useful, however those
;; measurement units are not standardized onto SI
;;
;; see also: 
;; http://crism.maden.org/consulting/pub/hytime/meas.html (1992)
;; http://www.is-thought.co.uk/schedule.htm




;;; % Geometry

(defgeneric scalar-measurement (instance))

(defclass* scalar ()
  ((measurement measurement)))



