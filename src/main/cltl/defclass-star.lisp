
(in-package #:math)
;: ^ FIXME: move into #:utils

(defmacro validate-class (class &optional (superclass 'standard-class))
  #+(or SBCL CMU CCL ALLEGRO)
  `(progn 
     (defmethod validate-superclass ((a ,class) (b ,superclass))
       (values t))
     (defmethod validate-superclass ((a ,superclass) (b ,class))
       (values t)))
  #-(or SBCL CMU CCL ALLEGRO)
  `(values))

(defun ensure-forward-referenced-class (name)
  (declare (type symbol name))
  (ensure-class name
		:direct-slots nil
		:direct-superclasses nil
		:metaclass (find-class 'forward-referenced-class)))

;; (ensure-forward-referenced-class (gensym))

(defmacro defclass* (name-form (&rest superclasses) 
                             (&rest slot-definitions)
                     &rest initargs)
  ;; FIXME: This macro does not, of itself, provide a complete
  ;; "DEFCLASS* SOMEWHAT LIKE DEFSTRUCT"  implementation. Features

  ;; presently lacking of this implementation, in that regards
  ;; may include:
  ;; 
  ;; * Many of the features avaialble for structure classes defined
  ;;   via DEFSTRUCT are altogether lacking from this protocol, such
  ;;   as: 
  ;;    * Naming and definition of a <type-p> predicate function
  ;;    * Naming and definition of a <copier> function
  ;;    * Specification of an object printer function
  ;;    * Naming and lambda list syntax for constructor functions
  ;;    * Nothing such as DEFSTRUCT's linear :INHERIT
  ;;    * Nothing such as a list type or vector type syntax for instances
  ;;    * Other qualities, as denoted in the following
  ;;
  ;; * In that this DEFCLASS* macro implements a syntax "Somewhat like
  ;;   defstruct", the following features are noted:
  ;;
  ;;   * NAME-FORM may be a symbol or a list. If a symbol, then
  ;;     NAME-FORM denotes the name of the class. If a list, then
  ;;     NAME-FORM denotes -- as its first element -- the name of the
  ;;     class, with a syntax similar to DEFTTRUCT <name-and-options>.
  ;;    
  ;;     DEFCLASS* emulates DEFSTRUCT' :CONC-NAME option
  ;;
  ;;   * For all direct slots of the class, a set of reader and writer
  ;;     methods will be defined, as named according to CONC-NAME
  ;;     interpreted in a manner similar to as with DEFSTRUCT (FIXME:
  ;;     However, in its present revisin, DEFCLASS* rather interprets
  ;;     a NULL CONC-NAME as a "flag" that no reader or writer methods
  ;;     should be defined). Unless the slot-definition is denoted as
  ;;     :READ-ONLY, a writer method will be defined for the slot
  ;;     definition, named as per <that naming convention>. In <all
  ;;     instances>, a reader method will be defined for the slot
  ;;     instances>definition (NOTE: Unless CONC-NAME is NULL

  ; FIXME: Redefine DEFCLASS* to interpret CONC-NAME in a manner more
  ; consissent onto DEFSTRUCT; provide an additional slot definition
  ; "flag" value for denoting if a slot defintion is not to have any
  ; reader or writer methods defined for it; revise this
  ; documentation, subsequent to that changeset. (Firstly, move
  ; DEFCLASS* into the mci-cltl-utils source tree). Lastly, describe
  ; the complete syntax for slot definition specifiers, as implemented
  ; of this DEFCLASS* macro -- and make a more direct reference to the
  ; X3J13 discussions surrounding the definition of the syntax of each
  ; of DEFSTRUCT and DEFCLASS in CLtL2 -- all of this, to proceed
  ; after implementation of Jetbrains' YouTrack and TeamCity
  ; components into a Glasfish server in an AWS instance.

  ;;
  ;; * When a class' slot is redefined from "not read only" to "read
  ;;   only" then the SLOT-DEFINITION-WRITER methods defined for slots
  ;;   in the class as side-effects of its definition as "not read
  ;;   only" are not undefined. Those "then invalid" writer methods
  ;;   should be made undefined, however, as consequent with the
  ;;   change in slot definition qualities
  ;;
  ;; * Concerning the :READ-ONLY value for slot specifiers for this
  ;;   macro, presently that value may seem to represent something of
  ;;   a misnomer. <Presently> :
  ;;
  ;;     * The READ-ONLY value is not stored with the slot
  ;;       definition - whether its direct slot definition of
  ;;       effective slot defintion
  ;;
  ;;     * The READ-ONLY value will not be inherited by subclasses
  ;;
  ;;     * The READ-ONLY value does not actually prevent SETF access
  ;;       to slots, as by way of (SETF SLOT-VALUE)
  ;;
  ;;   Those shortcomings may be addressed, subsequently, with a
  ;;   definition of a READ-ONLY-INSTANCE-SLOT-DEFINITION
  ;;   protocol. However, as one's experiences in developing
  ;;   extensions onto CLOS might seem to prove: Once the proverbial
  ;;   Pandora's box of slot definition extension is opened, then --
  ;;   in less figurative terms -- it may be difficult to develop any
  ;;   effectively "layered" extensions onto STANDARD-SLOT-DEFINITION,
  ;;   as well as the direct and effective slot definition subclasses
  ;;   of STANDARD-SLOT-DEFINITION. Certainly, that is not to
  ;;   criticise the design of MOP, whereas -- presently -- one
  ;;   considers that it may be possible to develop an extensional
  ;;   architecture for facilitating a definition of "layered",
  ;;   domain-specific slot definition extensions
  ;;
  ;;   As well as that such an architecture may be applied in
  ;;   developing this READ-ONLY-INSTANCE-SLOT-DEFINITION proposal,
  ;;   but furthtermore: Such an architecture may be applied for
  ;;   developing a MODALLY-LOCKED-SLOT-DEFINITION protocol namely
  ;;   using read-write locking onto slot values, as may be
  ;;   facilitative of thread safety in Common Lisp programs.
  ;;
  ;;   Thirdly, such an architecture may be applied for developing an
  ;;   L10N-SLOT-DEFINITION proposal, namely to facilitate
  ;;   internationalization of locale-specific values - especially,
  ;;   language-specific strings -- within Common Lisp applications.
  ;;   See also: The #I18N notes peppered throughout the codebases
  ;;   provided of the MetaCommunity.info project
  ;;
  ;;  Thus, effectively five "Architectural FIXME" items are defined here:
  ;;
  ;;  * DEFCLASS*-MORE-SOMEWHAT-LIKE-DEFSTRUCT
  ;;  * LAYRED-SLOT-DEFINITON-ARCHITECTURE
  ;;  * READ-ONLY-SLOT-DEFINITION
  ;;  * MODALLY-LOCKED-SLOT-DEFINITION
  ;;  * L10N-SLOT-DEFINITION liketly to be followed with
  ;;    I18N-STRING-SLOT-DEFINITION and a broader I18N protocol
  ;;    integrating with existing internationalization frameworks, e.g
  ;;    "PO files"
  (destructuring-bind (name &key (conc-name nil cnp))
      (cond
        ((symbolp name-form) (list name-form))
        (t name-form))

    (unless (or cnp conc-name)
      (setq conc-name (make-symbol (format nil "~A-" name))))

    (labels ((acc (slot read-only-p)
               (when conc-name
                 (list (if read-only-p :reader :accessor)
                       (intern-formatted "~A~A" conc-name slot))))
             (initarg (slot)
               (list :initarg (intern (symbol-name slot) :keyword)))

             (initform (form infp)
               (when infp
                 (list :initform form))))
             
      `(defclass ,name (,@superclasses) 
         ,(mapcar (lambda (spec)
                    (destructuring-bind (name &optional (type t)
                                              &key read-only
                                              (initform nil infp))
                        spec
                      `(,name ,@(acc name read-only)
                          ,@(initarg name)
                          :type ,type
                          ,@(initform initform infp))))
                  slot-definitions)
         ,@initargs))))

#+NIL
(macroexpand-1 (quote
(defclass* (frob :conc-name #:a-frob-) ()
  ((b)
   (c real :read-only t)))
))
