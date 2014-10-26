;; decimal-scale.lisp

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
           (values fixnum integer))
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
              (t (return (values n d))))))))))
    
#+NIL ;;instance tests
(labels ((frob-test (n)
           (multiple-value-bind (scale magnitude)
               (integer-shift-digits n)
             (values scale magnitude
                     (= n (* magnitude (expt 10 scale)))))))
  ;; (frob-test 1020)
  ;; => 1, 102, T
  
  ;; (frob-test 102)
  ;; => 0, 102, T

  ;; (frob-test 10)
  ;; => 1, 1, T

  ;; (frob-test 1)
  ;; => 0, 1, T

  ;; (frob-test 0)
  ;; => 0, 0, T
  )


;; FIXME: The syntax of INTEGER-SHIFT-DIGITS and FLOAT-SHIFT-DIGITS
;; is counterintuitive, namely in the ordering of the return values.
;; Both should => MAGNITUDE, SCALE

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

This funcdtion implements a calculation similar to a base-10
calculation of the significand and exponent of `d`."
  (declare (type (or integer float) d)
           (values fixnum integer))
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
            ;; => -3, 1201
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
            (multiple-value-bind (scale-shift magnitude)
                (integer-shift-digits scaled-magnitude)
              (return (values (- scale-shift n)
                              magnitude)))))
           (t (incf n)))))))

;; (float-shift-digits 1)
;; => 0, 1

;; (float-shift-digits 12)
;; => 0, 12
;; (float-shift-digits 12.0)
;; => 0, 12
;; (float-shift-digits 12.1)
;; => -1, 121
;;

;; (float-shift-digits 120.1)
;; => -1, 1201

;; (float-shift-digits pi)
;; => -15, 3141592653589793
;; ^ not all the same as (RATIONALIZE PI)

;; (float-shift-digits 12000)
;; => 3, 12

;; n.b: 
;; (* 10 314.1592653589793d0)
;; => 3141.5926535897934d0
;; ^ a decimal digit is introduced

;; (float-shift-digits 1.201d0)
;; => -3, 1201
;; ^ subtly works around a matter of floating point error
;;   i.e. in which (* 10  1.201d0)
;;                  => 12.010000000000002d0

;; (float-shift-digits 1.201)
;; => -3, 1201

;; (float-shift-digits 11.0d0)
;; 0, 11


;; n.b
;; (* 10 4.159265358979312d0)
;; => 41.592653589793116d0
;;
;; thus, towards numerical filtering, i.e. float-to-rational conversion
;;
;; (float-shift-digits 4.159265358979312d0)
;; => -15, 4159265358979312
;; previously, without the filtering, would => -15, 4159265358979311
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
* `rescale', `nrescale'

Notes:
* Results are undefined if DEGREE represents a prefix not available
  to `find-prefix'"
  (declare (type real magnitude)
           (type fixnum degree)
           (type measurement-class-designator unit)
           (values measurement))
  (let ((class (etypecase unit
                   (symbol (find-measurement-class unit))
                   (measurement-class unit))))
    (etypecase magnitude
      (ratio 
       (values (make-instance class :magnitude magnitude 
                              :degree degree)))
      (real 
       (multiple-value-bind (scale adj-magnitude)
           (float-shift-digits magnitude)
         (values (make-instance 
                  class
                  :magnitude adj-magnitude
                  :degree (+ degree scale))))))))

#+NIL
(let* ((m (make-measurement 1 :m))
       (m-2 (rescale m 3))
       (m-3 (rescale m -3)))
  (values  (apply #'= (mapcar #'scalar-magnitude 
                             (list m m-2 m-3)))))
;; => T

;; (make-measurement 1 :m 5)
;; => #<METER 100 km {1003FF93A3}>
;; (measurement-magnitude (make-measurement 1 :m 5))
;; => 1


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
;; => #<METER 1 kg {1006260FC3}>

;; (make-measurement 1 :kg 6)
;; => #<METER 1 Mkg {1006260FC3}> ;; INCORRECT


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
