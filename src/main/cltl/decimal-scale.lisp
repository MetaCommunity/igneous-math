;; decimal-scale.lisp - exponent-based rational storage for decimal values

(in-package #:math)

#|

 (defun total-integer-part-digits (n)
   (1+ (truncate (log n 10))))

 (total-integer-part-digits 1020.1)
 ;; => 4

 (total-integer-part-digits 102.1)
 ;; => 3

|#

(defun integer-shift-digits (d)
  "Perform a decimal shift of D to the minimum significant digits of D, returning the scale and the shifted magnitude

Example:

  (multiple-value-bind (scale magnitude)
    (integer-shift-digits 1020)
   (= (* magnitude (expt 10 scale)) 1020))
  => T
"
  (declare (type integer d)
           (values integer fixnum))
  (cond
    ((zerop d)
     (values 0 0))
    (t 
     (let ((n 0))
       (declare (fixnum n))
       (loop
          #+NIL (format t "~%FROB: ~s ~S" d n)
          (multiple-value-bind (a r)
              (truncate d 10) 
            (cond
              ((zerop r) (setq d a) (incf n))
              (t (return (values d n))))))))))
    
#+NIL ;;instance tests
(labels ((frob-test (n)
           (multiple-value-bind (magnitude scale)
               (integer-shift-digits n)
             (values scale magnitude
                     (= n (* magnitude (expt 10 scale)))))))
  ;; (frob-test 1020)
  ;; => 102, 1, T
  
  ;; (frob-test 102)
  ;; => 102, 0, T

  ;; (frob-test 10)
  ;; => 1, 1, T

  ;; (frob-test 1)
  ;; => 1, 0, T

  ;; (frob-test 0)
  ;; => 0, 0, T
  )


(defun float-shift-digits (d) 
"A floating point value, `d`, with a known number of significant decimal digits, `n`, may be represented as a sequence of rational values
   `(a, n)` for  `a = d * 10^n`. 

For example, with d=3.14, then n=-2, d=314

For rhetorical purposes, `a` may be denoted as the magnitude of
`d`, and `n` denoted as the scale of `d`.  

When `n` is known, then calculations evaluated on `d`, may instead be 
evaluated on `(a, n)`, with `a` and `n` both being of type, rational. 
In implementations of mathematical operations with this manner of
decimal scaling, it may serve to avoid some floating point errors, in
some implementations -- moreover, allowing for an implementation of
strictly rational calculations for mathematical operations.

This function implements a calculation similar to a base-10
calculation of the significand and exponent of `d`."
  (declare (type (or integer float) d)
           (values integer fixnum))
  (let ((b d)
        (n 1))
    (declare (type (or integer float) b)
             (type fixnum n) )
    (loop  
       #+NIL (format t "~%FROB.: ~s ~s" b n)
       (setq b (* b 10))
       (multiple-value-bind (b r) 
           (truncate b)
         (declare (ignore b))
         (cond
           ((zerop r) 
            ;; n.b: This effectively works around some matters
            ;; of floating point error, such as when 
            ;;
            ;; (* 10  1.201d0)
            ;;  => 12.010000000000002d0
            ;;
            ;; however
            ;; (float-shift-digits 1.201d0)
            ;; => 1201, -3
            (multiple-value-bind (scaled-magnitude r)
                ;; truncate so as to ensure an integral value is passed
                (truncate (* d (expt 10 n)))
              (unless (zerop r)
                ;; effectively, "Round up"
                ;;
                ;; e.g. when 
                ;;
                ;;   d = 4.159265358979312d0
                ;;   n = 15
                ;;
                ;; i.e
                ;;
                ;; (truncate (* 4.159265358979312d0 (expt 10 15)))
                ;;  => 4159265358979311, 0.5d0
                ;;
                ;; That may be a result of rounding in the floating
                ;; point implementation.
                ;;
                ;; Without the following adjustment, the containing
                ;; function would return an inaccurate value,
                ;; in that instance.
                (incf scaled-magnitude))
            (multiple-value-bind (magnitude scale-shift)
                (integer-shift-digits scaled-magnitude)
              (return (values magnitude
			      (- scale-shift n))))))
           (t (incf n)))))))

;; (float-shift-digits 1)
;; => 1, 0

;; (float-shift-digits 12)
;; => 12, 0
;; (float-shift-digits 12.0)
;; => 12, 0
;; (float-shift-digits 12.1)
;; => 121, -1
;;

;; (float-shift-digits 120.1)
;; => 1201, -1

;; (float-shift-digits pi)
;; => 3141592653589793, -15
;; ^ not all the same as (RATIONALIZE PI)

;; (float-shift-digits 12000)
;; => 12, 3

;; n.b: 
;; (* 10 314.1592653589793d0)
;; => 3141.5926535897934d0
;; ^ a decimal digit is introduced

;; (float-shift-digits 1.201d0)
;; => 1201, -3
;; ^ subtly works around a matter of floating point error
;;   i.e. in which (* 10  1.201d0)
;;                  => 12.010000000000002d0

;; (float-shift-digits 1.201)
;; => 1201, -3

;; (float-shift-digits 11.0d0)
;; => 11, 0


;; n.b
;; (* 10 4.159265358979312d0)
;; => 41.592653589793116d0
;;
;; thus, towards numerical filtering, i.e. float-to-rational conversion
;;
;; (float-shift-digits 4.159265358979312d0)
;; => 4159265358979312, -15
;; previously, without the filtering, would => 4159265358979311, -15
;;
;; for example:
;; (truncate (* 4.159265358979312d0 (expt 10 15)))
;; 4159265358979311, 0.5d0
;; ^ thus the additional filtering is defined

;; (float-shift-digits (rationalize 4.159265358979312d0))
;; ^ N/A (FIXME)

;; ...

;; (defstruct (decimal
;;              (:constructor %make-decimal (magnitude scale)))
;;   ;; FIXME: This reproduces MEASUREMENT and SCALAR
;;   (magnitude 0 :type fixnum)
;;   (scale 0 :type fixnum))

;; (defun make-decimal (n)
;;   (declare (type real n)
;;            (values decimal))
;;   (multiple-value-bind (scale magnitude)
;;       (float-shift-digits n)
;;     (%make-decimal magnitude scale)))

;; (defun decimal-value (d)
;;   (declare (type decimal d)
;;            (values real))
;;   (* (decimal-magnitude d)
;;      (expt 10 (decimal-scale d))))

;; ;; (make-decimal pi)
;;
;; (= (float (decimal-value (make-decimal pi)) most-positive-double-float) pi)
;; => T


(defun make-measurement (magnitude unit &optional (degree 0))
  "Crate a MEAUREMENT object of the specified MAGNITUDE of a decimal
scale denoted by DEGREE, representing a scalar measurement of
the measurement unit denoted by UNIT. 

Examples:

  (make-measurement 1 :m)
  => #<METER 1 m {1006289003}>

 (make-measurement 1 :m -3)
 => #<METER 1 mm {10062E90C3}>


See also: 
* `scalar-magnitude'
* `prefix-of'
* `rescale', `nrescale'"
  (declare (type real magnitude)
           (type fixnum degree)
           (type measurement-class-designator unit)
           (values measurement))
  (let ((class (etypecase unit
                   (symbol (find-measurement-class unit))
                   (measurement-class unit))))
    ;; FIXME: This does not explicitly scale the {MAGNITUDE, DEGREE}
    ;; down to the base measurement unit. 

    ;; In an optimized implementation, all measurement values may be
    ;; scaled to integer/magnitude+exponent values onto the base
    ;; measurement unit -- thus, allowing for direct application of
    ;; formulas likewise defined onto the base measurement unit,
    ;; without further magnitude+exponent scaling -- essentially,
    ;; leaving any conversion from/to non-base measurement units to the
    ;; input/display components of the implementation.
    ;;
    ;; Presently, this system applies a mehtodology essentialy of
    ;; "Scaling to significant digits".
    (etypecase magnitude
      (ratio 
       (values (make-instance class :magnitude magnitude 
                              :degree degree)))
      (real 
       (multiple-value-bind (adj-magnitude scale)
           (float-shift-digits magnitude)
         (values (make-instance 
                  class
                  :magnitude adj-magnitude
                  :degree (+ degree scale))))))))

;; (make-measurement 1 :m 5)
;; => #<METER 100 km {1003FF93A3}>
;; (measurement-magnitude (make-measurement 1 :m 5))
;; => 1

;; Example: Scaling to significant digits
;;
;; (measurement-magnitude (make-measurement 103 :m 10))
;; => 103
;; (measurement-degree (make-measurement 103 :m 10))
;; => 10
;;
;; (measurement-magnitude (make-measurement 1030 :m 9))
;; => 103
;; (measurement-degree (make-measurement 1030 :m 9))
;; => 10

;;- Ratio magnitude (unscaled)
;; (make-measurement 1/5 :m)
;; => #<METER 100 km {1003FF93A3}>
;; (measurement-magnitude (make-measurement 1/5 :m))
;; => 1/5
;;
;; (make-measurement 1/5 :m 3)
;; => #<METER 1/5 km {1007B81023}>
;; (measurement-magnitude (make-measurement 1/5 :m 3))
;; => 1/5
;;
;; (scalar-magnitude (make-measurement 1/5 :m 3))
;; => 200 ;; i.e. 200 m
;;
;; (scalar-magnitude (make-measurement 1/5 :m))
;; => 1/5 ;; i.e. 1/5 m


;; (make-measurement 1 :kg)
;; => #<KILOGRAM 1 kg {1006260FC3}>

;; (make-measurement 1 :kg 6)
;; => #<KILOGRAM 1 Mkg {1006260FC3}> ;; INCORRECT


;; (make-measurement 1 :m 3)
;; => #<METER 1 km {1003FF93A3}>

;; (measurement-magnitude (make-measurement 1 :m 3))
;; => 1
;; (measurement-degree (make-measurement 1 :m 3))
;; => 3

;; (measurement-magnitude (make-measurement 1000 :m 3))
;; => 1
;; (measurement-degree (make-measurement 1000 :m 3))
;; => 6

;; (measurement-print-name (make-measurement 1 :m))
;; => "m"
