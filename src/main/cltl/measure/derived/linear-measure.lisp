;; linear-measure.lisp - linear measurement units/expressions

(in-package #:math)

(defclass linear-measurement-class (derived-measurement-class)
  ((degree
    :initarg :degree
    :type fixnum
    :accessor measurement-degree)))

;; LINEAR-UNIT-EXPR : defined in measurement-base.lisp
