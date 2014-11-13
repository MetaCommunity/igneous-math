;; linear.lisp - matrix operations

(in-package #:math)


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

(defun row (m i)
  (declare (type array-dimension-designator i)
           (type (array t (* *)) m)
           (values (array t (1 *)) m))
  (let* ((n-cols (array-dimension m 1))
         (reslt (make-array (list 1 n-cols)
                            :element-type (array-element-type m))))
    (dotimes (n n-cols reslt)
      (setf (aref reslt 0 n) 
            (aref m i n))
    )))

;; (row #2A((1 2 3 4) (4 3 2 1)) 0)
;; => #2A((1 2 3 4))


(defun column (m j)
  (declare (type array-dimension-designator j)
           (type (array t (* *)) m)
           (values (array t (1 *)) m))
  (let* ((n-rows (array-dimension m 0))
         (reslt (make-array (list n-rows 1)
                            :element-type (array-element-type m))))
    (dotimes (n n-rows reslt)
      (setf (aref reslt n 0) 
            (aref m n j))
    )))

;; (column #2A((1 2 3 4) (4 3 2 1)) 0)
;; => #2A((1) (4))


#+TO-DO ;; depends-on : ov-seq.lisp
(defun gaussian-reduction (m)
  ;; cf. pages.pacificcoast.net/~cazelais/251/gauss-jordan.pdf

  ;; defunct prototype

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


      (dotimes (row n-rows 
                #-TO-DO vector-form
                #+TO-DO retv)
        (let* ((refr (aref vector-form row)))

          (dotimes (col (1- n-cols))
            (unless (= col row)
              (let* ((col-value (aref refr col)))
              ;; reduce following rows to zero
                (dotimes (%row-next (- n-rows (1+ row)))
                  (let* ((row-next (+ %row-next (1+ row)))
                         (this-row (aref vector-form row-next))
                         (factor (gcd col-value (aref this-row col)))
                         (factored-refr (@* factor refr))
                         (result  (@- this-row factored-refr)))
                    (declare (type (simple-array t (*)) vector-form))
                    (dotimes (n (1- n-cols))
                      (setf (aref refr n)
                            (aref result n)))))
                ))))))))

;; (gaussian-reduction #2A((1 1 1 5) (2 3 5 8) (4 0 5 2)))

;; (gaussian-reduction #2A((21 1 1 5) (2 3 5 8) (4 0 5 2)))

;; (gaussian-reduction #2A((1 1 2 0 1) (2 -1 0 1 -2) (2 -1 -1 -2 4) (2 -2 2 -1 0)))


;; displaced-index-offset
