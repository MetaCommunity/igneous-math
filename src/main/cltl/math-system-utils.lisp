;; math-system-utils.lisp - misc. utility forms

(in-package #:mcicl.math)


(defmacro vsubsetp (v1 v2 &key (key nil kp)
                            (test #'eql tp) 
                            (test-not nil tnp))
  ;; cf. subsetp
  (with-gensym (%v1 %v2 %elt %test %test-not)
    `(let ((,%v1 ,v1)
           (,%v2 ,v2)
           ,@(when tp `((,%test ,test)))
           ,@(when tnp `((,%test-not ,test-not))))
       (declare (type (simple-array t (*)) ,%v1 ,%v2))
       (do-vector (,%elt ,%v1 t)
         (unless (find ,%elt ,%v2 
                       ,@(when kp `(:key ,key))
                       ,@(when tp `(:test ,%test))
                       ,@(when tnp `(:test-not ,%test-not)))
           (return nil))))))

;; (vsubsetp #(4 2) #(2 3 4) :test #'=)
;; => T 

;; (vsubsetp #(4 8) #(2 3 4) :test #'=)
;; => NIL
