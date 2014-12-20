;; geom-measure.lisp - geometric measurement units/expressions

(in-package #:math)

(defclass geometric-measurement-class (derived-measurement-class)
  ((degree
    :initarg :degree
    :type fixnum
    :accessor measurement-degree)))

(defclass geometric-unit-expr (linear-unit-expr)
  ((degree
    :initarg :degree
    :accessor unit-expr-degree
    :type fixnum)))
