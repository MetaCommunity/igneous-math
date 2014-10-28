;; math-system-utils.lisp - misc. utility forms

(in-package #:math)

(define-condition entity-not-found (error)
  ((name
    :initarg :name
    :reader entity-not-found-name)))

(define-condition redefinition-condition (style-warning)
  ((previous-object
    :initarg :previous
    :accessor redefinition-condition-previous-object)
   (new-object
    :initarg :new
    :accessor redefinition-condition-new-object)))
