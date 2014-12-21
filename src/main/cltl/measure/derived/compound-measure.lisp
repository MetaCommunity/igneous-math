;; compound-measure.lisp - object model for compound measurements

(in-package #:math)


(defgeneric measurement-unit-sequence (class))

(defgeneric measurement-degree-sequence (class))

(defgeneric measurement-normalized-expression (class))


;; % Compound Measurements - Registry and Retrieval

#| Notes

* This system will define a registry specifically for compound
  measurement unit classes, effectively augmenting the registry of
  base and derived measurement unit classes  comprised of
  `%MEASUREMENT-CLASSES%' and its corresponding
  `%MEASUREMENT-CLASSES-LOCK%'

* At time of registering a componound measurement class, the class may
  be indexed within the compound measurement unit classes registry,
  according to three primary values:

    0. The identity of the class, itself. Assuming that the Common
       Lisp implementation ensures that any class Cn named N (n > 1)
       will always be EQ to class C1 named N, the identity of the
       class Cn may be applied as an index key, such that the
       functions storing and accessing the index may use EQ
       as a :test function.

    1. The MEASUREMENT-NORMALIZED-EXPRESSION defined to the
       measurement unit class. Although this may seem to denote a 
       cosmetically appealing method for functional query onto the
       compound measurement unit classes registry, however it may not
       present the most optimal approach possible.

    2. The set of the unit sequence and degree sequence for the
       normalized measurement unit expression defined to
       the measurement unit class. It may serve to permit for an
       optimized implementation in the functions for measurement query
       and unit conversion, when these values would be accessed
       seperately.

* When searching for a measurement unit class, provided an arbitrary
  "normalized" base measurement unit expression, it may be assumed:

    * that the elements of the expresion may occur in any arbitrary 
      order. 
    * that a subset of the elements of the expression may effectively
      "match" the complete set of elements of a registered, compound
      measurement unit class
    * that the expression may contain elements in addition to any
      subset of elements effectively "matching" the normalized unit
      expression of any registered, compound measurement unit class

|#

;; % Compound Measurements - Measurement Class Definition and Initialization


(defclass compound-measurement-class (measurement-class)
  ((unit-sequence
    :initarg :unit-sequence
    :type (simple-array measurement-class (*))
    :reader measurement-unit-sequence)
   (degree-sequence
    :initarg :degree-sequence
    :type (simple-array fixnum (*))
    :reader measurement-degree-sequence)
   (normalized-expression
    :type (simple-array t (* 2))
    :initarg :normalized-expression
    :reader measurement-normalized-expression
    )))
  

(defmethod shared-initialize :around ((instance compound-measurement-class)
                                      slots &rest initargs 
                                      &key expression
                                        &allow-other-keys)
  (declare (type list expression))
  (let (args-changed-p)
    (when expression
      (labels ((make-buffer (length element-type)
                 (make-array length
                             :element-type element-type))
               (normalize (expr)
                 ;; NOTE: It's assumed that the :EXPRESSION value
                 ;; would be provided in base measurement units.
                 ;; So, this function does not expressly normalize 
                 ;; to base measurement units. 
                 ;;
                 ;; See also: `NORMALIZE-UNIT', `SIMPLIFY-UNIT'
                 (declare (type list expr))
                 (let* ((%expr (coerce expr 'simple-vector))
                        (n (length (the simple-vector %expr)))
                        (reslt (make-array (list n 2)
                                           :element-type t)))
                   (dotimes (index n reslt)
                     (let* ((elt (aref %expr index))
                            (s (etypecase elt
                                 (symbol elt)
                                 (cons (car elt))))
                            (c (find-measurement-class s))
                            (d (etypecase elt
                                 (symbol 1)
                                 (cons (cadr elt)))))
                       (declare (type measurement-class c)
                                (type fixnum d))
                       (setf (aref reslt index 0) c
                             (aref reslt index 1) d)))))
               (unit-of (expr index)
                 (declare (type (simple-array t (* 2))
                                expr))
                 (aref expr index 0))
               (map-units (expr)
                 (declare (type (simple-array t (* 2))
                                expr))
                 (let* ((n (array-dimension expr 0))
                        (buffer (make-buffer n 'measurement-class)))
                   (dotimes (index n buffer)
                     (let ((elt (unit-of expr index)))
                       (declare (type measurement-class elt))
                       (setf (aref buffer index) elt)))))
               (degree-of (expr index)
                 (declare (type (simple-array t (* 2))
                                expr))                 
                 (aref expr index 1))
               (map-degrees (expr)
                 (declare (type (simple-array t (* 2))
                                expr))                 
                 (let* ((n (array-dimension expr 0))
                        (buffer (make-buffer n 'fixnum)))
                   (dotimes (index n buffer)
                     (let ((elt (degree-of expr index)))
                       (declare (type fixnum elt))
                       (setf (aref buffer index) elt))))))
        (let ((nex (normalize expression)))          
          (setq args-changed-p t)
          (setf (getf initargs :normalized-expression) nex
                (getf initargs :unit-sequence) (map-units nex)
                (getf initargs :degree-sequence) (map-degrees nex)))))
    (cond 
      (args-changed-p (apply #'call-next-method instance slots initargs))
      (t (call-next-method)))))

;; To Do: Develop prototypes for SIMPLIFY-UNIT / NORMALIZE-UNIT
;; with 'Newton' and 'N s' (momentum) as examples


#+PROTOTYPE
(defclass force (derived-measurement-class
                 compound-measurement-class)
  ()
  (:print-name . "force")
  (:print-label . "force")
  (:symbol . :force)
  (:metaclass measurement-domain)
  (:base-measure . newton))

#+PROTOTYPE
(register-measurement-domain 'force)


#+PROTOTYPE
(defclass newton (measurement)
  ()
  (:print-name . "newton")
  (:print-label . "N")
  (:symbol . :|N|)
  (:metaclass 
   compound-measurement-class ;; FIXME: is not 'force'
   )
  (:expression :|m| :|kg| (:|s| -2)))

#+NIL ;; instance test
(measurement-domain-base-measure (find-class 'force))
;; => #<COMPOUND-MEASUREMENT-CLASS NEWTON>

#+PROTOTYPE
(register-measurement-class 'newton)

#+PROTOTYPE
(resister-compound-measurement-class 'newton)

;; (measurement-unit-sequence (find-class 'newton))
;; => #(#<LENGTH METER> #<MASS KILOGRAM> #<TIME SECOND>)

;; (measurement-degree-sequence (find-class 'newton))
;; => #(1 1 -2)

;; (measurement-normalized-expression (find-class 'newton))
;; => #2A((#<LENGTH METER> 1) (#<MASS KILOGRAM> 1) (#<TIME SECOND> -2))

(defclass compound-unit-expr (unit-expr)
  ((elements
    :initarg :elements
    :accessor unit-expr-elements
    :type (simple-array t (* 2)))))


;; NORMALIZE-UNIT - Prototype

(defun normalize-unit-expression (&rest exprs)
  (declare (values (simple-array t (* 2))))
  (let* ((%exprs (coerce exprs 'simple-vector))
         (n (length %exprs))
         (buffer (make-array n :fill-pointer 0)))
    (declare (type simple-vector %exprs))
    (dotimes (index n (make-array (list (length buffer) 2)
                                  :initial-contents buffer))
      (let* ((elt (aref %exprs index))
             (s (etypecase elt
                  (symbol elt)
                  (cons (car elt))))
             (c (find-measurement-class s))
             (d (etypecase elt
                  (symbol 1)
                  (cons (cadr elt)))))
        (declare (type measurement-class c)
                 (type fixnum d))
        (labels ((find-elt (s buffer)
                   (do-vector (elt buffer (values nil nil))
                     (let ((elt-s (aref elt 0)))
                       (when (eq s elt-s)
                         (return  (values (aref elt 1) elt))))))
                 (ensure-elt (c d buffer)
                   (let ((s (measurement-symbol c)))
                   (multiple-value-bind (%d elt) 
                       (find-elt s buffer)
                     (cond
                       (%d (setf (aref elt 1) (+ %d d)))
                       (t  (vector-push-extend (vector s  d)
                                               buffer)))))))
        (etypecase c
          (base-measurement-class
           (ensure-elt c d buffer))
          (linear-measurement-class
           ;; FIXME: To convert to base units from a non-base linear
           ;; measurement class will require a measurement conversion,
           ;; such that a measurement value must be available.
           ;;
           ;; Note that this would apply to all non-base measurements,
           ;; though it's rather kludged for now.
           (warn "Not converting non-base linear measurement ~A" c)
           (ensure-elt c d buffer))
          (geometric-measurement-class
           (let ((%d (measurement-degree c)))
             (ensure-elt c (* d %d) buffer)))
          (compound-measurement-class
           (let ((nex (measurement-normalized-expression c)))
             (dotimes (%index (array-dimension nex 0))
               (let ((%c (aref nex %index 0))
                     (%d (* d (aref nex %index 1))))
                 (ensure-elt %c %d buffer)))))))))))

;; NORMALIZE-UNIT / SIMPLIFY-UNIT - Usage cases

;; Prototype: Newtonian force / Weight / Acceleration due to gravity (Units)
;;
;; (normalize-unit-expression :|N|)
;; => #2A((:|m| 1) (:|kg| 1) (:|s| -2))
;;
;; (normalize-unit-expression :|m| :|kg| '(:|s| -2))
;; => #2A((:|m| 1) (:|kg| 1) (:|s| -2))
;;
;; (normalize-unit-expression '(:|N| 2))
;; => #2A((:|m| 2) (:|kg| 2) (:|s| -4)) ;; (FIXME: Verify)


;; (simplify-unit-expression :|m| :|kg| '(:|s| -2))

;; Prototype: Momentum (Units)
;;
;; (normalize-unit-expression :|N| :|s|)
;; =initially=> #2A((:|m| 1) (:|kg| 1) (:|s| -2) (:|s| 1))
;; =revised=> #2A((:|m| 1) (:|kg| 1) (:|s| -1))
;;
;; (simplify-unit :|m| :|kg| (:|s| -2) :|s|)
;; (simplify-unit :|kg| :|m| (:|s| -1))
