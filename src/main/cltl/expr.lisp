;; expr.lisp - measurement unit expressions

(in-package #:math)

(defgeneric unit-expr-unit (expr))
(defgeneric (setf unit-expr-unit) (new-value expr))

(defgeneric unit-expr-degree (expr))
(defgeneric (setf unit-expr-degree) (new-value expr))

(defgeneric unit-expr-elements (expr))
(defgeneric (setf unit-expr-elements) (new-value expr))

(defclass unit-expr ()
  ())

(defclass linear-unit-expr (unit-expr)
  ((unit
    :initarg :unit
    :accessor unit-expr-unit
    :type measurement-class)))

(defclass geometric-unit-expr (linear-unit-expr)
  ((degree
    :initarg :degree
    :accessor unit-expr-degree
    :type fixnum)))

(defclass compound-unit-expr (unit-expr)
  ((elements
    :initarg :elements
    :accessor unit-expr-elements
    :type simple-vector)))


