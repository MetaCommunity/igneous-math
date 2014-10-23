;; prefix.lisp

(in-package #:math)


(defgeneric rescale (scalar prefix)
  (:documentation 
   "Return a new scalar object representing the magnitude of SCALAR 
multiplied by the effective factor-base of the measurement rasied to
the PREFIX degree.

For measurements using SI decimal prefixes, the effective factor base
is 10

See also: `nrescale'"))


(defgeneric nrescale (scalar prefix )
  (:documentation 
   "Return the SCALAR object, such that the SCALAR-MAGNITUDE of the
SCALAR will be equivalent to its orignal value, though PREFIX will be
used newly as the SCALAR-PREFIX of the SCALAR. Methods specialized on
this function will alter the SCALAR by side-effect.

The SCALAR-MAGNITUDE of a SCALAR is calcualted as the magnitude of
SCALAR  multiplied by the effective factor-base of the measurement
rasied to the PREFIX degree. For measurements using SI decimal
prefixes, the effective factor base is 10.

See also: `rescale'"))

;; nb. Methods for RESCALE and NRESCALE are defined after the class
;; PREFIX is defined


;;; % Prefix Notation - SI Decimal Prefix Notation, specifically

;; NB: "Engineering notation" typically uses only prefixes for degrees
;; in multiples of 3

(defgeneric prefix-degree (instance)
  (:method ((instance measurement))
    (measurement-degree instance)))

(defgeneric prefix-print-label (instance)
  (:method ((instance measurement))
    (prefix-print-label (prefix-of instance))))

(defgeneric prefix-print-name (instance)
  (:method ((instance measurement))
    (prefix-print-name (prefix-of instance))))

(defgeneric prefix-symbol (instance)
  (:method ((instance measurement))
    (prefix-symbol (prefix-of instance))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant %si-degrees%
    (cond
      ((boundp '%si-degrees%)
       (values %si-degrees%))
      (t 
       (make-array 21
                   :element-type '(integer -24 24)
                   :initial-contents
                   '(24 21 18 15 12 9 6 3 2 1 
                     0
                     -1 -2 -3 -6 -9 -12 -15 -18 -21 -24))
      ))))


(deftype prefix-degree ()
  '(and (integer -24 24)
    ;; formally onto the SI standard for decimal prefixes
    #.(cons 'member (coerce %si-degrees% 'list))))

;; (typep -5 'prefix-degree)
;; => NIL
;; (typep 9 'prefix-degree)
;; => T


(defun find-nearest-degree (deg &optional ee-p)
  ;; ee-p : "prefer engineering notation" 
  ;; i.e. for a non-zero degree, 
  ;; require that the prefix degree is a multiple of 3
  (declare (type fixnum deg)
           (values prefix-degree))
  (cond
    ((zerop deg) (values 0))
    ((> deg 24) (values 24))
    ((< deg -24) (values -24))
    (t 
     (let ((n (unless ee-p
                (find deg %si-degrees% :test #'=))))
       (cond
         (n (values n))
         (t 
          (labels ((test (a b)
                     (declare (type fixnum a)
                              (type (integer -24 24)
                                    b))
                     (cond
                       (ee-p 
                        (and (= (gcd b 3) 3)
                             (> a b)))
                       (t (> a b)))))
            (let ((n (position deg %si-degrees%
                               :test #'test)))
              (aref %si-degrees% n)))))))))

;; (find-nearest-degree 0)
;; => 0

;; (find-nearest-degree 25)
;; => 24

;; (find-nearest-degree -25)
;; => -24

;; (find-nearest-degree 5)
;; => 3

;; (find-nearest-degree -5)
;; => -6

;; (find-nearest-degree -2)
;; => -2
;; (find-nearest-degree -2 t)
;; => -3


;; (find-nearest-degree 1)
;; => 1
;; (find-nearest-degree 1 t)
;; => 0


(defun scale-for-si-degree (magnitude scale &optional ee-p)
  (declare (type real magnitude)
           (type fixnum scale)
           (values real prefix-degree))
  (cond
    ((zerop scale) 
     (values magnitude scale))
    (t 
     (let ((deg (find-nearest-degree scale ee-p)))
       (values (* magnitude (expt 10 (- scale deg)))
               deg)
       ))))

;; (scale-for-si-degree 1 5)
;; => 100, 3
;; (= 1E+05 100E+03)

;; (scale-for-si-degree 1 -5)
;; => 10, -6
;;
;; (= 1E-05 10E-06)
;; => T

;; (scale-for-si-degree 1 -2)
;; => 1, -2

;; (scale-for-si-degree 1 -2 t)
;; => 10, -3


;; (scale-for-si-degree 1 2)
;; => 1, 2

;; (scale-for-si-degree 1 2 t)
;; => 100, 0
;;
;; (= (* 1 (expt 10 2)) (* 100 (expt 10 0)))
;; => T



(defclass* prefix ()
  ;; NOTE: This class is applied effectively as a DECIMAL-PREFIX 
  ;; FIXME: Rename PREFIX class to DECIMAL-PREFIX,
  ;; FIXME: Implement class PREFIX-CLASS with accessor PREFIX-BASE
  ;; FIXME: Implement class DECIMAL-PREFIX-CLASS with PREFIX-BASE 10
  ;; FIXME: Implement class OCTAL-PREFIX-CLASS with PREFIX-BASE 8
  ;; FIXME: Implement a measurement-domain for information quantity,
  ;;        supporting an OCTAL-PREFIX-CLASS for prefixes thereof.
  ;; FIXME: Seperately, impelement a measurement-domain for 
  ;;         virtual (e.g. display screen, cf HyTime) measurements  
  ((degree prefix-degree :read-only t)
   (print-label simple-string :read-only t)
   (print-name simple-string :read-only t)
   (symbol symbol :read-only t)))


;; FIXME: Define classes DECIMAL-PREFIX, BINARY-PREFIX
;; as well as accessor PREFIX-FACTOR-BASE
;;
;; Hypothetically, also consider defining an experimental range of
;; prefixes: LOGARITHMIC-PREFIX <ABSTRACT>, DECIMAL-LOGARITHMIC-PREFIX,
;; and NATURAL-LOGARITHMIX-PREFIX cf. frequency domain analysis,
;; with documentation clearly and succinctly describing the syntax and
;; characteristics of the same measurement domain (perhaps to a
;; discrete sense of technical Goodwill for  MetaCommunity, if well
;; done) as well as its origins in frequency domain analysis of
;; electrical systems -- also with an xref onto CLIM, then, as with
;; regards to graphs of ideal frequency response in analysis of
;; electrical systems components (use cases? radio communication
;; systems namely onto HAM radio standards; audio-digital converstion
;; components; power factor analysis for industrial-quality electrical
;; systems with a big, red, blinking HTML label for safety analysis.)

(defmethod print-object ((instance prefix) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (princ (slot-value* instance 'print-label "{no label}") 
           stream)))


(define-condition prefix-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "No measurement prefix registered for name ~S"
             (entity-not-found-name c)))))

(define-condition prefix-degree-not-found (entity-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "No measurement prefix registered for degree ~S"
             (entity-not-found-name c)))))


(declaim (type simple-vector %prefixes%))
(defvar %prefixes% (make-array 20))


(defun find-prefix (s)
  (declare (type symbol s)
           (values prefix))
  (or (find s %prefixes%
            :test #'eq
            :key #'prefix-symbol)
      (error 'prefix-not-found :name s)))

;; (find-prefix :|m|)


(defun find-prefix= (d)
  (declare (type prefix-degree d)
           (values prefix))
  (or (find d %prefixes%
            :test #'=
            :key #'prefix-degree)
      (error 'prefix-degree-not-found
             :name d)))

;; (find-prefix= -9)
;;  <<nano>>
           



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
            ;; FIXME: Update the documentation (README.md namely) as
            ;; to denote the syntax for measurement symbols, applied
            ;; here - with a sidebar note as with regards to readtable
            ;; case within a Common Lisp programming system
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
            (micro  -6 "Î¼" :|u|)
            (nano  -9 "n" :|n|)
            (pico  -12 "p" :|p|)
            (femto  -15 "f" :|f|)
            (atto  -18 "a" :|a|)
            (zepto  -21 "z" :|z|)
            (yocto  -24 "y" :|y|)))))


(defmethod print-object ((object measurement) stream)
  (print-unreadable-object (object stream :type t :identity t)

    (multiple-value-bind (mag boundp)
        (slot-value* object 'magnitude)
      (cond 
        (boundp
         (multiple-value-bind (deg boundp)
             (slot-value* object 'degree)
           (cond
             (boundp
              (multiple-value-bind (adj-mag deg)
                  (scale-for-si-degree mag deg)
                  (princ adj-mag stream)
                  (write-char #\Space stream)
                (unless (zerop deg)
                  (let ((prefix (find-prefix= deg))) 
                    (princ (prefix-print-name prefix) stream)))))
             (t (princ mag stream)))))
        (t
         (princ "{no magnitude}" stream))))
    
    (princ (measurement-print-name (class-of object))
           stream)))


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


(defun prefix-of (m)
  ;; FIXME: Memoize the PREFIX value for MEASUREMENT instance
  (declare (type measurement m)
           (values prefix fixnum))
  (let ((deg (measurement-degree m))
        (mag (measurement-magnitude m)))
    (multiple-value-bind (mag-adj deg-adj)
        (scale-for-si-degree mag deg)
      (let ((prefix
             (find deg-adj %prefixes%
                   :key #'prefix-degree
                   :test #'(lambda (a b)
                             (declare (type prefix-degree a b))
                             (= a b)))))
        (values prefix mag-adj)))))

;; (prefix-of (make-measurement 1 :m 24))
;; => <<yotta>>, 1

;; (prefix-of (make-measurement 1 :m 23))
;; --> <<zetta>>, 100

;; Trivial decimal exponential mathematics, ad hoc syntax:
;;
;;  1 m => 1000 mm
;;      => 0.001 km

;;
;; syntax: 
;;   (<magnitude>, <degree>)
;;   <A> =[<decimal shift>]=> <B>
 ;;  <=>   i.e. "equivalent to"
;; 
;; (1, 0) =[3]=> (.001, 3) <=> (1 * 10^-3, 3)
;; (1, 0) =[-3]=> (1000, -3) <=> (1 * 10^3, -3)
;;
;; e.g in a conventional syntax
;;  1 m = .001 km
;;  1 m = 1000 mm

#+NIL ;; algorithm test
;; 1. define an implicit measurment instance via LET
;;
;; 2. return values representative of the original implicit
;;    measurement instance, as "decimal shifted" for a new decimal
;;    prefix degree
;;
(let ((magnitude 1)
      (degree 0)
      (factor-base 10))
  (flet ((decimal-shift (new-degree)
           (cons (* magnitude (expt factor-base (- degree new-degree)))
                 (+ degree new-degree))))
    (values (decimal-shift 3)
            (decimal-shift -3))))
  

(defun shift-magnitude (magnitude degree new-degree 
                        &optional (factor-base #.%factor-base%))
  ;; utility function - implementation of the "decimal shift"
  ;; algorithm denoted in the above
  (declare (type real magnitude)
           (type fixnum degree new-degree factor-base))
  (values (* magnitude (expt factor-base (- degree new-degree)))
          (+ degree new-degree)))


(defmethod rescale ((scalar measurement) (prefix fixnum))
  (multiple-value-bind (new-mag new-deg) 
      (shift-magnitude (measurement-magnitude scalar)
                       (prefix-degree scalar)
                       prefix
                       #.%factor-base%)
    (make-measurement new-mag  (class-of scalar)
                      new-deg)))

(defmethod rescale ((scalar measurement) (prefix prefix))
  (rescale scalar (prefix-degree prefix)))


(defmethod nrescale ((scalar measurement) (prefix fixnum))
  (multiple-value-bind (new-mag new-deg) 
      (shift-magnitude (measurement-magnitude scalar)
                       (prefix-degree scalar)
                       prefix
                       #.%factor-base%)
    (setf (measurement-degree scalar) new-deg)
    (setf (measurement-magnitude scalar) new-mag)
    (values scalar)))

(defmethod nrescale ((scalar measurement) (prefix prefix))
  (nrescale scalar (prefix-degree prefix))
  (values scalar))

#+NIL ;; instance test - rescale, equivalent magnitude
(let* ((m (make-measurement 1 :m))
       (m-2 (rescale m 3))
       (m-3 (rescale m -3)))
  (values m m-2 m-3
          (mapcar #'scalar-magnitude 
                  (list m m-2 m-3))
          (apply #'= (mapcar #'scalar-magnitude 
                             (list m m-2 m-3)))))

;; => #<METER 1 m {1006330FD3}>, 
;;    #<METER 1/1000 km {1006331AF3}>, ;; "RATIO QUIRK"
;;    #<METER 1 m {1006332293}>, 
;;    T 
;; ^ Implementation note: The magnitude of the measurement is 1,
;;   in each of those three instances. 
;;
;;   The convenient, printed representation -- as illustrated -- may
;;   seem to suggest as though a floating-point value was used in the
;;   third instance. In the third instances, in the previous, the
;;   measurement's magnitude is 1 and its degree is -3. At no time
;;   does any of those instanes use a floating-point object in its
;;   implementation. 
;;
;;   Though, of course, floating-point values would occur throughout a
;;   mathematical system, in application, but insofar as discrete
;;   ratios and integral values may be utilized within mathematical
;;   calculations -- insofar as of integral and ratio values serving
;;   as mathematically accurate alternatives to mathematically
;;   equivalent floating point values -- then perhaps it may serve
;;   towards a greater development of a basis for systematic
;;   measurement accuracy, within mathematical systems extending of
;;   this system, that a simple magnitude/degree model is developed --
;;   as in  preference to a completely "dot decimal" model for
;;   expression of measuremnt values, within digital information
;;   objects. Of course, this model is developed as an extension of
;;   the subset of ANSI Common Lisp implemented as with regards to
;;   numeric types and mathematical procedures.
;;
;;   In a sense, it is a goal in the design of this system that
;;   numbers will be applied without truncation, insofar as towards
;;   calculation of "end values". So far as procedures for addition,
;;   multiplication, subtraction, and divsion may be conducted
;;   thoroughly with integral and ratio values, then such integral
;;   and ratio values may be used iternally, within program objects. 
;;   A floating point notation may be applied, nonetheless, for
;;   printed representation of objects' numeric values, in a
;;   conventional syntax. 
;;
;;   Of course, as in calculations effectively applying the ANSI CLtL
;;   definition of 'pi', it may be fortuitous to extend on ANSI CLtL
;;   with a methdology similar to the the 'thunk'[1] in which the
;;   evaluation of the constant 'pi' would be delayed until
;;   construction of a comprehensive formula for calculation of an
;;   "end value" within a mathematical system. See also: Garnet KR[2]
;;
;; [1] http://www.chadbraunduin.com/2011/07/common-lisp-lazy-sequences.html
;; [2] http://sourceforge.net/projects/garnetlisp/files/

#+NIL ;; instance test - nrescale
(let* ((m (make-measurement 1 :m))
       (m-2 (nrescale m 3)))
  (values m-2 (eq m-2 m) (= 1 (scalar-magnitude m))))

;; => #<METER 1/1000 km {1005F4C4D3}>, 
;;    T, T


