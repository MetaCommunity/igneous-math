;; compound-measure.lisp - object model for compound measurements

(in-package #:math)


(defconstant* +si-base-measurements+
    (make-array 7
                :initial-contents
                (mapcar #'find-class
                        '(length mass time electrical-current
                          temperature amount-substance
                          luminous-intensity))))



(defclass compound-measurement-class (measurement-class)
  ((unit-sequence
    :initarg :unit-sequence
    :reader measurement-class-unit-sequence)
   (degree-sequence
    :initarg :degree-sequence
    :reader measurement-class-degree-sequence)))


(defclass compound-unit-expr (unit-expr)
  ((elements
    :initarg :elements
    :accessor unit-expr-elements
    :type simple-vector)))

