;; ov-seq.lisp - overloading for simple mathematical operations onto Lisp sequence objects

(in-package #:math)

#+NIL
(defmethod @= ((a list) (b list))
  )

#+NIL
(defmethod @= ((a simple-vector) (b simple-vector))
  )


;; trivial example:

(defmethod @* ((a real) (b vector))
  ;; NOTE: This assumes B is a vector of numbers
  (let* ((len (length b))
         (retv (make-array len
                           :element-type (array-element-type b))))
    (declare (type array-dimension-designator  len))
    (dotimes (n len retv)
      (setf (aref retv n) (* a (aref b n)))
      )))

;; (@* 5 #(4 0 5 2))
;; => #(20 0 25 10)

;; (@* 1 #(4 0 5 2))
;; => #(4 0 5 2)

#+SCALAR_TO-DO
(defmethod @* ((a scalar) (b vector))
  (let* ((len (length b))
         (retv (make-array len
                           :element-type (array-element-type b))))
    (declare (type array-dimension-designator  len))
    (dotimes (n len retv)
      (setf (aref retv n) (@* a (aref b n)))
      )))



(defmethod @- ((a vector) (b vector))
  ;; NOTE: This assumes A, B are vector of numbers
  (let* ((len (length b))
         (retv (make-array len
                           :element-type (array-element-type b))))
    (declare (type array-dimension-designator  len))
    (dotimes (n len retv)
      (setf (aref retv n) (- (aref a n) (aref b n)))
      )))


;; (@- #(20 0 25 10) #(4 0 5 2))
;; => #(16 0 20 8)


;;  Trivial application: see linear.lisp
