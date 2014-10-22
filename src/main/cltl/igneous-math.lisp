
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
               (list :initarg (intern (symbol-name slot) :keyword)))

             (initform (form infp)
               (when infp
                 (list :initform form))))
             
      `(defclass ,name (,@superclasses) 
         ,(mapcar (lambda (spec)
                    (destructuring-bind (name &optional (type t)
                                              &key read-only
                                              (initform nil infp))
                        spec
                      `(,name ,@(acc name read-only)
                          ,@(initarg name)
                          :type ,type
                          ,@(initform initform infp))))
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


(define-condition class-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "No measureent class registered for name ~S"
             (entity-not-found-name c)))))

   

#+SBCL ;; FIXME: also port to other impmls. using PCL
(defmethod sb-mop:validate-superclass ((class measurement-class)
                                       (superclass standard-class))
  (values t))


(let ((%classes% (make-array 7 :fill-pointer 0))
      (%classes-lock% (make-lock "%CLASSES%")))
  (defun register-measurement-class (c)
    (declare (type measurement-class c))
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
          (error 'class-not-found :name s))))
  )
  



(defgeneric measurement-magnitude (instance))

(defgeneric measurement-degree (instance))


(defclass* measurement ()
    ((magnitude real :initform 0)
     ;; degree : an exponent of 10
     (degree fixnum :initform 0)))

(defun base-magnitude (m)
  (declare (type measurement m)
           (values real))
  (let ((deg (measurement-degree m)))
    (cond 
      ((zerop deg) (measurement-magnitude m))
      (t (* (measurement-magnitude m)
            (expt 10 deg))))))

(defgeneric magnitude-for-degree (measurement degree)
  (:method ((m measurement) (deg fixnum))
    (let ((next-deg (+ deg (measurement-degree m))))
      (cond
        ((zerop next-deg) (measurement-magnitude m))
        (t (* (measurement-magnitude m)
              (expt 10 next-deg)))))))

;; (magnitude-for-degree (make-measurement 1 :m 3) 0)
;; => 1000

;; (magnitude-for-degree (make-measurement 1 :m 3) -3)
;; => 1


;;; % Prefix Notation 

;; NB: "Engineering notation" typically uses only prefixes for degrees
;; in multiples of 3

(defgeneric prefix-degree (instance))
(defgeneric prefix-print-label (instance))
(defgeneric prefix-print-name (instance))
(defgeneric prefix-symbol (instance))


(deftype prefix-degree ()
  '(integer -24 24))

(defclass* prefix ()
  ((degree prefix-degree)
   (print-label simple-string)
   (print-name simple-string)
   (symbol symbol)))


(defmethod print-object ((instance prefix) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (princ (slot-value* instance 'print-label) stream)))

  (define-condition entity-not-found (error)
  ((name
    :initarg :name
    :reader entity-not-found-name)))
  

(define-condition prefix-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "No measureent prefix registered for name ~S"
             (entity-not-found-name c)))))

(define-condition prefix-degree-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "No measureent prefix registered for degree ~S"
             (entity-not-found-name c)))))


(declaim (type simple-vector %prefixes%))
(defvar %prefixes% (make-array 22))


(defun find-prefix (s)
  (declare (type symbol s)
           (values prefix))
  (or (find s %prefixes%
            :test #'eq
            :key #'measurement-symbol)
      (error 'prefix-not-found :name s)))


;; define prefix classes
(let ((n -1))
  (labels ((do-def (p degree print-name name)
             (let ((p (make-instance 'prefix
                                     :symbol name
                                     :degree degree
                                     :print-name print-name
                                     :print-label (string-downcase (symbol-name p))
                                     )))
               (setf (svref %prefixes% (incf n)) p)
               (values p))))
    (mapcar (lambda (spec)
              (destructuring-bind 
                    (c  degree print-name name) spec
                (do-def c degree print-name name)))
          '((yotta 24 "Y" :|Y|)
            (zetta 21 "Z" :|Z|)
            (exa 18 "E" :|e|)
            (peta  15 "P" :|p|)
            (tera  15 "T" :|t|)
            (giga  9 "G" :|g|)
            (mega  6 "M" :|M|)
            (kilo  3 "k" :|k|)
            (hecto  2 "h" :|h|)
            (deca  1 "da" :|da|)
            (deci  -1 "d" :|d|)
            (centi  -2 "c" :|c|)
            (milli  -3 "m" :|m|)
            (micro  -6 "μ" :|u|)
            (nano  -9 "n" :|n|)
            (pico  -12 "p" :|p|)
            (femto  -15 "f" :|f|)
            (atto  -18 "a" :|a|)
            (zepto  -21 "z" :|z|)
            (yocto  -24 "y" :|y|)))))




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



(defmethod print-object ((object measurement) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (multiple-value-bind (mag boundp)
        (slot-value* object 'magnitude)
      (princ (cond
               (boundp mag)
               ;; FIXME: #I18N
               (t "{no magnitude}"))
             stream))

    (multiple-value-bind (deg boundp)
        (slot-value* object 'degree)
      (cond
        (boundp (unless (zerop deg)
                  (format stream "E~@D" deg)))
        (t (write-char #\Space stream)
           ;; FIXME: #I18N
           (princ "{no degree}" stream))))

    (write-char #\Space stream)

    (princ (measurement-print-name (class-of object))
             stream)))

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

(defun make-measurement (magnitude unit &optional (degree 0))
  (declare (type real magnitude)
           (type class-designator unit))
  ;; note: consequences are undefined if DEGREE represents a prefix 
  ;; not indexed in %PREFIXES%
  (make-instance (find-measurement-class unit)
                 :magnitude magnitude
                 :degree degree))

;; (make-measurement 1 :m)

;; (make-measurement 1 :m 3)

;; (measurement-print-name (make-measurement 1 :m))
;; => "m"

(defun prefix-of (m)
  (declare (type measurement m)
           (values prefix))
  (let ((deg (measurement-degree m)))
    (or  (find deg %prefixes%
               :key #'prefix-degree
               :test #'(lambda (a b)
                         (declare (type prefix-degree a b))
                         (= a b)))
         (error 'prefix-degree-not-found :name deg))))


;; (prefix-of (make-measurement 1 :m 24))
;; => <<yotta>>

;;; referencing http://physics.nist.gov/cuu/pdf/sp811.pdf




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



