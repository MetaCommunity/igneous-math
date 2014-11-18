;; math-system-utils.lisp - misc. utility forms

(in-package #:math)

(define-condition entity-condition ()
  ((name
    :initarg :name
    :reader entity-condition-name)))


(define-condition entity-not-found (error entity-condition)
  ())

(define-condition redefinition-condition (style-warning)
  ((previous-object
    :initarg :previous
    :accessor redefinition-condition-previous-object)
   (new-object
    :initarg :new
    :accessor redefinition-condition-new-object)))


(define-condition container-condition ()
  ((container
    :intarg :container
    :reader container-condition-container)))
