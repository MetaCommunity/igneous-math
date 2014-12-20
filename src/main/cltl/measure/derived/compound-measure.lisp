;; compound-measure.lisp - object model for compound measurements

(in-package #:math)


(defconstant* +si-base-measurements+
    (make-array 7
                :initial-contents
                (mapcar #'find-class
                        '(length mass time electrical-current
                          temperature amount-substance
                          luminous-intensity))))


