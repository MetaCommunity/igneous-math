;; info.metacommunity.cltl.math.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:math-system
    (:use #:asdf #:cl)))

(in-package #:math-system)


(defsystem #:info.metacommunity.cltl.math
  :description 
  "Mathematical sysetm in Common Lisp"
  :version "1.0"
  ;; :homepage "https://github.com/MetaCommunity/igneous-math"
  ;; :license "https://github.com/MetaCommunity/igneous-math/blob/master/LICENSE"
  :depends-on 
  (#:info.metacommunity.cltl.utils
   #:closer-mop
   #:bordeaux-threads)
  :serial t
  :components 
  ((:file "math-package")
   (:file "defclass-star") ;; FIXME: move into .utils
   (:file "measurement")
   (:file "decimal-scale")
   (:file "prefix")
   ))
