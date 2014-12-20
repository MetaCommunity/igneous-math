;; compound-measure.lisp - object model for compound measurements

(in-package #:math)


(defconstant* +si-base-measurements+
    (make-array 7
                :initial-contents
                (mapcar #'find-class
                        '(length mass time electrical-current
                          temperature amount-substance
                          luminous-intensity))))


(defgeneric measurement-class-unit-sequence (class))

(defgeneric measurement-class-degree-sequence (class))

(defgeneric measurement-class-normalized-expression (class))

(defclass compound-measurement-class (measurement-class)
  ((unit-sequence
    :initarg :unit-sequence
    :type (simple-array measurement-class (*))
    :reader measurement-class-unit-sequence)
   (degree-sequence
    :initarg :degree-sequence
    :type (simple-array fixnum (*))
    :reader measurement-class-degree-sequence)
   (normalized-expression
    :type (simple-array t (* 2))
    :initarg :normalized-expression
    :reader measurement-class-normalized-expression
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
                     (let* ((elt (pop expr))
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
(defclass newton (measurement)
  ()
  (:print-name . "newton")
  (:print-label . "N")
  (:symbol . :|N|)
  (:metaclass compound-measurement-class)
  (:expression :|m| :|kg| (:|s| -2))
  (:documentation "Measurement class for quanities \
of force in base unit newton (N)

Symbolic representation: :N"))

;; (measurement-class-unit-sequence (find-class 'newton))
;; => #(#<LENGTH METER> #<MASS KILOGRAM> #<TIME SECOND>) [2 times]

;; (measurement-class-degree-sequence (find-class 'newton))
;; => #(1 1 2)

;; (measurement-class-normalized-expression (find-class 'newton))
;; => #2A((#<LENGTH METER> 1) (#<MASS KILOGRAM> 1) (#<TIME SECOND> -2))

(defclass compound-unit-expr (unit-expr)
  ((elements
    :initarg :elements
    :accessor unit-expr-elements
    :type simple-vector)))


