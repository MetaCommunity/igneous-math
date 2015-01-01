;; parse-tree.lisp - generic parser framework

;; see also: math-reader.lisp

;; FIXME: Move this code into an indep system in the mci-cltl-utils source tree

#| Documentation, draft nr. 1

From a perspective of the _data space_ in a software program, the
igneous-math _prefix_ and _measurement-class_ objects are defined,
each, in its own respective namespace.  From a perspective of a
conventional measurement unit syntax, however, the _prefix_ and
_measurement-class_ objects all  occupy the same namespace,
effectively.  

Thus, it might seem that one could develop a uniform _parse tree_ for
_prefix_ and _measurement-class_ objects -- updating the _parse tree_,
as it being representative of the _string syntax namespace_, when any
new _measurement-class_ or _prefix_ value would be registered to its
respective _object-model namespace_. 

Programmatically it becomes a non-trivial endeavor, but some
documentation about it might serve to clear that up, somehow. 

It might need something of an illustration -- that would be a break
from writing so many _(lisp (code (software (lines)))_ of 

So, continuing in context, with an illustration:

given: _S_ =_{{"k", <prefix "k">} {"m" <prefix "m"> <measurement-class "meter">}_
 
function("m", _S_ ) should => 0, <measurement-class "meter">
function("mm", _S_) should => -3, <measurement-class "meter">

In the instance of parsing the string "m", the _function_ may need to
_prefer_ a _measurement-class_ to a _prefix_ object, as in this
instance in which "m" is the designator both of a _prefix_ object and
a _measurement-class_, within the _parse tree, S_. 

In the instance of parsing the string, "mm", the _function_ should
firstly arrive at the _prefix_ "m", then parsing the same _S_ -- but
in the second time, effectively ignoring _prefix_ type objetcs --
should arrive at the _measurement-class_ "m", in completing the
computation. 

The respective digit value would be derived from the the respective
<prefix>, if any is registered. 

I see, now, that it doesn't need to be a hundreds-deep node tree, at
least for that simple example onto fundamental _measurement units_
objects. 

The  _parser architecture_ also needs to be _scalable_,  in a
sense --  e.g. onto inputs such as "m^2" representing degrees of a
given base unit of measure -- for an arbitrary base unit of measure
and an arbitrary degree, as such -- and such as "V" (volt) vs "W/A" vs
"m^2 kg s^-3 A^-1", the last of which representing a derived unit as a 
relation of base units, in a composition more complex than 
e.g. "m^2"

...and it needs to have "Ambiguity detection," as well as to present
"Redefinition warnings," when "appropriate" 

Pursued to a logical end, Igneous-Math has a partly functional
measurement unit module, already, but it can be fairly tedious to
interact with. The _parse-tree_ item, pictured, would be defined
simply for supporting a  convenient input syntax for measurement
objects, within the broader Igneous-math system. 


|#


(in-package #:mcicl.math)

;;; % Condition Classes

(define-condition node-condition ()
  ((node 
    :initarg :node
    :reader node-condition-node)))

(define-condition node-not-found (entity-not-found node-condition )
  ()
  (:report 
   (lambda (c s)
     (format s "No node ~S defined as subnode of ~`S"
	     (entity-condition-name c)
	     (node-condition-node c)))))

(define-condition node-exists (entity-condition container-condition 
						node-condition)
  ()
  (:report 
   (lambda (c s)
     (format s "Node ~S already defined for ~S within ~S"	     
	     (node-condition-node c)
	     (entity-condition-name c)
	     (container-condition-container c)
	     ))))

(define-condition node-exists-error (error node-exists)
  ())

(define-condition node-redefinition (redefinition-condition container-condition
							    node-condition)
  ()
  (:report 
   (lambda (c s)
     (format s "Redefining ~S with ~S (previosuly ~S) within ~S"
	     (node-condition-node c)
	     (redefinition-condition-new-object c)
	     (redefinition-condition-previous-object c)
	     (container-condition-container c)))))

;;; % Node class and basic protocol

(defclass* node ()
  ((character character)
   (subnodes (vector node)
	     :initform (make-array 0 :fill-pointer 0
				   :element-type 'node))))

(defun make-node (c)
  (declare (type character c))
  (values (make-instance 'node :character c)))


(defun finalize-parse-tree (node)
  (declare (type node node)
	   (values node))
  (with-accessors ((subnodes node-subnodes)) node
    (setf subnodes (simplify-vector subnodes))
    (do-vector (%node subnodes node)
      (finalize-parse-tree %node))))

;;; %% 'Parse' interface onto Node trees

(defun parse (str tree &key (start 0) error-p)
  (declare (type string str)
	   (type node tree)
	   (type array-dimension-designator start)
	   (values (or node null) array-dimension-designator))
  (let ((len (length str)))
    (when (>= start len)
      (simple-program-error 
       ;; FIXME: #I18N
       "~<At node ~S parsing ~S:~> ~<Start ~S >= string length ~S~>" 
       tree str start len))
  (with-slot-lock-held (parent subnodes :READ) 
    (let* ((c (char str start))
	   (node (find c (node-subnodes :test #'char=
					:key #'node-character))))
      (cond
	(node (incf start)
	      (cond
		((= start len) (values node (1- start)))
		(t (parse str node start))))
	(error-p (error 'node-not-found :name c :node tree))
	(t (values nil start)))))))

;;; % Node Registry interface

(defun register-node (string node parent &optional redefine-p)
  (declare (type string string)
	   (type node node parent)
	   (values node))
  (let ((c (node-character node)))
    ;; FIXME: HERE, THE SUBNODES SLOT VALUE LOCKED, MUST BE
    ;; so, this needs the instance-locking slot value extension to DEFINE

    (multiple-value-bind (%node index)
	(parse string parent :error-p nil)
      (cond
	((and %node redefine-p) ;; FIXME: ONLY: IF (= (+1 INDEX) (LENGTH STRING))
	 (warn 'node-redefinition 
	       :new node
	       :previous %node
	       :node c
	       :container parent)
	 (let* ((n (position %node (node-subnodes parent)
			     :test #'eq)))
	   ;; WHY THE LOCK HELD, IT.
	   ;; ONLY :WRITE LOCK THE WHERE IT NEEDS HELD, IT.
	   ;; DEFINE, MUST, THE :READ :WRITE LOCKNG EXTENSION, FIRSTLY
	   ;; ^ FIRSTLY BEFORE, THEN LOCK SLOT: DO.
	   ;; ^ ^ THEN HERE, THE LOCK :WRITE DO
	   (with-slot-lock (parent subnodes :write)
	     ;; FIXME: WRITE ABOUT RECURSIVE TIMING CONDITIONS FOR LOCKING IN THIS, A TREE-SHAPED, CHARACTER-INDEXED PARSING ALGORITHM, "NOVEL"
	     (setf (aref (node-subnodes parent) n) node))))
	(%node
	 (error 'node-exists-error :name c :node %node :container node))
	(index
	 ;; initialize generic NODE instances
	 ;; starting at (SCHAR STRING INDEX)
	 ;; continuing to limit INDEX=(LENGTH STRING)
	 (ERROR "Not complete, this branch. Program arrived here, and ~S ~S ~S ~S"
		index %node node parent)
	 )
	))))
#+NIL
(defvar %tree% (make-node #\f))
