;; compound-measure.lisp - object model for compound measurements

(in-package #:math)

#+NIL
(defconstant* +si-base-measurement-metaclasses+ ;; (?)
    ;; This constant value is defined here, in order to ensure that
    ;; all of the base measurement unit classes are defined at the
    ;; time when this form is evaluated
    (make-array 7
                :initial-contents
                (mapcar #'find-class
                        '(base-length 
                          base-mass 
                          base-time 
                          base-electrical-current
                          base-temperature 
                          base-amount-substance
                          base-luminous-intensity))))

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
                (getf initargs :degree-sequence) (map-degrees nex))))
      (cond 
        (args-changed-p (apply #'call-next-method instance slots initargs))
        ;; FIXME: Thie following CALL-NEXT-METHOD form should probably
        ;; not be denoted as 'unreachable code' by the compiler.
        ;; In SBCL 1.2.5.76-65a44db, it is denoted as unreachable
        ;; however. 
        (t (call-next-method))))))

;; To Do: Develop prototypes for SIMPLIFY-UNIT / NORMALIZE-UNIT
;; with 'Newton' and 'N s' (momentum) as examples


#+PROTOTYPE
(defclass force (derived-measurement-class)
  ()
  (:print-name . "force")
  (:print-label . "force")
  (:symbol . :force)
  (:documentation 
   "Measurement domain for quantities of force")
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
  (:metaclass compound-measurement-class)
  (:expression :|m| :|kg| (:|s| -2))
  (:documentation "Measurement class for quantities \
of force in base unit newton (N)

Symbolic representation: :N"))

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
        (flet ((add-elt (c d buffer)
                 ;; FIXME: CONSY buffer implementation
                 (vector-push-extend (list (measurement-symbol c) d) 
                                     buffer)))
        (etypecase c
          ((or base-measurement-class 
               linear-measurement-class
               geometric-measurement-class)
           (add-elt c d buffer))
          (compound-measurement-class
           (let ((nex (measurement-normalized-expression c)))
             (dotimes (%index (array-dimension nex 0))
               (let ((%c (aref nex %index 0))
                     (%d (* d (aref nex %index 1))))
                 (add-elt %c %d buffer)))))))))))

;; NORMALIZE-UNIT / SIMPLIFY-UNIT - Usage cases

;; Prototype: Newtonian force / Weight / Acceleration due to gravity (Units)
;;
;; (normalize-unit-expression :|N|)
;; => #2A((:|m| 1) (:|kg| 1) (:|s| -2))
;;
;; (normalize-unit-expression :|m| :|kg| '(:|s| -2))
;; => #2A((:|m| 1) (:|kg| 1) (:|s| -2))

;; (simplify-unit-expression :|m| :|kg| '(:|s| -2))

;; Prototype: Momentum (Units)
;;
;; (normalize-unit-expression :|N| :|s|)
;; (simplify-unit :|m| :|kg| (:|s| -2) :|s|)
;; (simplify-unit :|kg| :|m| (:|s| -1))
