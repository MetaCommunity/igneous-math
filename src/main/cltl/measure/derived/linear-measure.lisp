;; linear-measure.lisp - linear measurement units/expressions

(in-package #:math)

(defclass linear-derived-measurement-class (derived-measurement-class
                                            linear-measurement-class )
  ())

;; LINEAR-UNIT-EXPR : defined in measurement-base.lisp
