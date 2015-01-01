;; math-reader.lisp - ...

(in-package #:mcicl.math)


(defun read-unit (s)
  (declare (type stream s)
	   (values (or prefix null)
		   measurement-class))
  (let* ((name (read-name-string s))
	 (len (length name)))
    (declare (type simple-string name))
    (case len
      ;; FIXME: Interning arbitrary strings in :KEYWORD creates an
      ;; effective memory leak. 
      ;;
      ;; Implement FIND-MEASUREMENT-CLASS-USING-NAME, and use that
      ;; function here
      (1 (values nil (find-measurement-class (intern name '#:keyword))))
      (t 
       ;; 1: parse for prefix
       ;; 2: parse for unit
       ;; 3. return measurement
       ;;
       (let ((maybe-prefix (read-characters s %prefix-length-limit% 
					    :eof-error-p nil)))
	 ;; FIXME: This needs a grammar of some kind?
	 ;; Note, however, the effective grammer is defined by the
	 ;; ordered set of: the set of registered prefixes and the set
	 ;; of registered measurements.

	 ;; So, must:
	 ;; A) Implement a parse-tree framework
	 ;; B) Within REGISTER-MEASUREMENT and REGISTER-PREFIX, update
	 ;;    the parse-tree with the OBJECT-PRINT-LABEL and the
	 ;;    INSTANCE of any any registered measurement or prefix
	 ;;    object 
	 ;; C) Apply that parse tree framework, here
	 ;;
	 ;; For each node in the parse tree, possible subnodes:
	 ;;  <parse-tree-node>
	 ;;  <prefix-class>
	 ;;  <measurement-class>
	 ;;
	 ;; If both a <measurement-class> and a <prefix-class> would
	 ;; be regisered as a subnode of a given node N, then that
	 ;; would denote an ambiguity onto N, and an error must be
	 ;; signaled before the second of the two would be regisetered
	 ;;
	 ;; Then, consider implementing the same parse-tree framework
	 ;; also onto a URI parser.
	 
	 ;;
	 ;; 1: Read a character
	 ;; 2? Compute whether the character denotes a prefix or a
	 ;; measurement value
	 ;; 2A: If prefix: goto 1
	 ;; 2B: If measurement, then:
	 ;; 2B1: Find named prefix
	 ;; 2C1: read-unit
	 ;; 3: Make measurement
	 ;; 4: Return
	 (error "PARSER ALGORITHM NOT IMPLEMENTED")) ;; "fail"
       ))))

(defun read-measurement (s)
  (declare (type stream s)
	   (values measurement &optional))
  (let ((mag (read s)))
    ;; FIXME: provide CERROR form for ASSERT
    (assert (numberp mag))
    (let ((c (peek-char t s nil)))
      (cond
	(c
	 (multiple-value-bind (prefix unit-class)
	     (read-unit s)
	   (cond
	     (prefix (setq prefix (prefix-degree prefix)))
	     (t (setq prefix 0)))
	   (make-measurement mag unit-class prefix)))
	(t ;; null C
	 (make-measurement mag (find-class 'unity)))))))

(defun read-measurement-from-string (str)
  (declare (type string str)
	   (values measurement &optional))
  (with-input-from-string (s str)
    (read-measurement s)))

;; (read-measurement-from-string "1 m")
;; => #<METER 1 m {...}>

;; (read-measurement-from-string "1.5E+03 km")

;; (read-measurement-from-string "1 km")



;; (read-measurement-from-string "1 m^3")
;; (read-measurement-from-string "1 mm^3")
