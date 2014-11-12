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


#| Trivial application:

(defun copy-array (m)
  (declare (type array m)
           (values array))
  (let* ((dims (array-dimensions m)) 
         (rm-len (apply #'* dims))
         (retv (make-array dims 
                           :element-type (array-element-type m))))
    (declare (type cons dims))
    (dotimes (n rm-len retv)
      (setf (row-major-aref retv n)
            (row-major-aref m n)))))

(defun compute-array-vector (m)
  ;; FIXME: Move this DEFUN to the MCi CLtL 'utils' system
  (declare (type array m)
           (values (simple-array t (*))))
  (let* ((len (apply #'* (the cons (array-dimensions m))))
         (retv (make-array len :element-type (array-element-type m))))
    (dotimes (n len retv)
      (setf (aref retv n)
            (row-major-aref m n)))))

;; (compute-array-vector #2A((1 2 3 4) (4 3 2 1)))
;; => #(1 2 3 4 4 3 2 1)

;; (compute-array-vector #3A(((1 2 3 4) (4 3 2 1)) ((4 3 2 1) (1 2 3 4))))
;; => #(1 2 3 4 4 3 2 1 4 3 2 1 1 2 3 4)


(defun compute-array-vector* (m)
  ;; FIXME: Move this DEFUN to the MCi CLtL 'utils' system
  (declare (type array m)
           (values (simple-array t (*))))
  (let ((dims (array-dimensions m)))
    (labels ((frob (m dims &optional upper-offsets)
               (let ((this-dim (car dims))
                     (rest-dim (cdr dims)))
                 (cond 
                   (rest-dim
                    (let ((retv 
                           (make-array this-dim :element-type 'vector)))
                      (dotimes (n this-dim retv)
                        (setf (aref retv n)
                              (frob m rest-dim 
                                    ;; FIXME: much consing
                                    (append upper-offsets
                                            (list n)))))))
                   (t 
                    (let ((retv
                           (make-array this-dim
                                       :element-type (array-element-type m))))
                      (dotimes (n this-dim retv)
                        (setf (aref retv n)
                              (row-major-aref 
                               m  
                               (apply #'array-row-major-index
                                      m 
                                      ;; FIXME: much consing
                                      (append upper-offsets
                                              (list n))))))))))))
      (frob m dims))))

;; (compute-array-vector* #2A((1 2 3 4) (4 3 2 1)))
;; => #(#(1 2 3 4) #(4 3 2 1))

;; (compute-array-vector* #3A(((1 2 3 4) (4 3 2 1)) ((4 3 2 1) (1 2 3 4))))
;; => #(#(#(1 2 3 4) #(4 3 2 1)) #(#(4 3 2 1) #(1 2 3 4)))


(defun gaussian-reduction (m)
  ;; cf. pages.pacificcoast.net/~cazelais/251/gauss-jordan.pdf
  (declare (type (array t (* *)) m)
           (inline copy-array)
           #+TO-DO (values vector))
  (let* ((n-rows (array-dimension m 0))
         (n-cols (array-dimension m 1))
         (rm-len (* n-rows n-cols))
         (vector-form (compute-array-vector* m))
         (retv (make-array (array-dimensions m)
                           :element-type (array-element-type m))))
    (declare (type array-dimension-designator n-rows n-cols)
             (type (simple-array t (*)) vector-form))
    (labels ((nth-element (mv row col)
               (declare (type array-dimension-designator posn)
                        (type (simple-array t (*)) mv)
                        (values number))
               (aref (aref mv row) col)))
      
      (let ((first-row (aref vector-form 0)))
        (dotimes (nr n-rows 
                  #-TO-DO vector-form
                  #+TO-DO retv)
          (cond
            ;; initial prototype - reduction in the first column, 
            ;; for rows 2...n
            ((zerop nr))
            (t (let ((this-row (aref vector-form nr)))
                 (declare (type (simple-array t (*)) this-row first-row))
                 (dotimes (col 1)
                   (let ((reslt (@- this-row 
                                    (@* (aref this-row col)
                                        first-row ))))
                     (dotimes (n n-cols)
                       (setf (aref this-row n)
                             (aref reslt n))
                       )))
                 ))))))))

;; (gaussian-reduction #2A((1 1 1 5) (2 3 5 8) (4 0 5 2)))

;; (gaussian-reduction #2A((1 1 2 0 1) (2 -1 0 1 -2) (2 -1 -1 -2 4) (2 -2 2 -1 0)))



|#
