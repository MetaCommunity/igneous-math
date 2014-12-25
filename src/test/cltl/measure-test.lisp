
;; ...

;; To Do: scan the entire codebase for inline tests, 
;;        and refactor for application here
;;
;; e.g. 

;; (measurement-domain-base-measure (domain-of (make-instance 'gram)))
;; => #<BASE-MASS KILOGRAM>

;; see also: SCALE-SI methods defined in prefix.lisp
;;   esp. SCALE-SI (MEASUREMENT &OPTIONAL T)

;; (make-measurement 1 :|kg|)
;; =should=> #<KILOGRAM 1 kg {10082A9083}> ;; OK

;; (scale-si (make-measurement 1 :|kg|))
;; => 1, 0 ;; OK - SCALE-SI KILOGRAM

#+NIL
(let ((m (base-convert-measurement (make-measurement 1 :|kg|))))
  (values (measurement-magnitude m)
          (measurement-degree m)))
;; => 1, 0 ;; OK - BASE-CONEVERT-MEASUREMENT 1 KG


;; (make-measurement 1 :|g|)
;; =should=> #<GRAM 1 g {...}> ;; FAIL - PRINT-LABEL GRAM

;; (scale-si (make-measurement 1 :|g|))
;; => 1000, -3 ;; OK - SCALE-SI 1 GRAM => 1000 * 10^-3 KG (SCALED)

;; see also: PRINT-LABEL (MEASUREMENT STREAM)

#+NIL
(let ((m (make-measurement 1 :|g|)))
  (values (measurement-magnitude m)
          (measurement-degree m)))
;; => 1, 0 ;; OK - MAKE-MEASUREMENT, MEASUREMENT INITIALIZATION, MEASUREMENT PROPERTIES

#+NIL
(let ((m (base-convert-measurement (make-measurement 1 :|g|))))
  (values (measurement-magnitude m)
          (measurement-degree m)))
;; => 1/1000, 0 ;; OK - BASE-CONVERT-MEASUREMENT 1 GRAM = 1/1000 KG (SCALED)

;; (make-measurement 1000 :|g|)
;; => #<GRAM 1000 g {10075C92B3}> ;; OK

;; (scale-si (make-measurement 1000 :|g|))
;; => 1000, 0 ;; FAIL - SCALE-SI 1000 GRAM => 1 * 10^0 KG (SCALED)
;; =SHOULD=> 1, 0

#+NIL
(let ((m (base-convert-measurement (make-measurement 1000 :|g|))))
  (values (measurement-magnitude m)
          (measurement-degree m)))
;; => 1, 0 ;; OK - BASE-CONVERT-MEASUREMENT 1000 GRAM => 1 KG (SCALED)



;; (convert-measurement (make-measurement 1 :|kg|)  :|g|)
;; =should=> #<GRAM 1000 g {...}> ;; FAIL

;; (convert-measurement (make-measurement 1000 :|g|)  :|kg|)
;; =should=> #<KILOGRAM 1 kg {...}> ;; FAIL - PRINT-LABEL MEASUREMENT STREAM ?

;; (measurement-magnitude (convert-measurement (make-measurement 1 :|kg|)  :|g|))
;; => 1
;; (measurement-degree (convert-measurement (make-measurement 1 :|kg|)  :|g|))
;; => 3 ;; OK

;; (base-convert-measurement (make-measurement 1 :|g|))
;; =should=> #<KILOGRAM 1 g {...}> ;; OK

;; (measurement-magnitude (base-convert-measurement (make-measurement 1 :|g|)))
;; => 1/1000 ;; OK 

;; (base-magnitude (make-measurement 1 :|g|))
;; =should=> 1/1000 ;; OK


;; (find-conversion-factor :|g| :|kg|)
;; => #<CONVERSION-FACTOR (1 g) => (1/1000 kg)> ;; OK
;; (find-conversion-factor :|kg| :|g|)
;; => #<CONVERSION-FACTOR (1 kg) => (1000 g)> ;; OK


