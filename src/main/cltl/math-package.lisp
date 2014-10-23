;; math-package.lisp

(in-package #:cl-user)

(defpackage #:info.metacommunity.cltl.math
  (:nicknames #:math)
  (:shadowing-import-from 
   ;; prefer the implementation's own forms to those defined in c2mop
   #:cl
   #:defgeneric
   #:defmethod
   #:standard-generic-function)
  
  (:use #:info.metacommunity.cltl.utils
        #:bordeaux-threads
        #:c2mop
        #:cl))


