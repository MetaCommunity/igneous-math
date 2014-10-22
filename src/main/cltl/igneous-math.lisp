
(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (dolist (s '(#:unit-formulas #:info.metacommunity.cltl.utils))
    (asdf:operate 'asdf:load-op s))

  (defpackage #:info.metacommunity.cltl.math
    (:use #:info.metacommunity.cltl.utils
          #:unit-formulas
          #:cl)
    
    (:import-from 
     #:unit-formulas 
     #:unit)
    )
  )


(in-package #:info.metacommunity.cltl.math)

;; Sidebar: Measurement definitions in HyTime
;;
;; cf. 
;; http://www.cs.utexas.edu/users/novak/units95.html
;;
;; http://www.bipm.org/en/publications/si-brochure/metre.html
;;  -> http://www.bipm.org/en/publications/si-brochure/download.html
;;
;; http://www.is-thought.co.uk/schedule.htm
;; http://crism.maden.org/consulting/pub/hytime/meas.html
;; http://www.hytime.org/materials/hi2mdhyt.sgm
;; ^ cf %HyGrand, "HyTime Granule Definition Notation"


;; see also:
;; http://physics.nist.gov/cuu/Units/
;; http://physics.nist.gov/Pubs/SP811/appenB.html 
;; ^ esp. for conversions regarding foot, mile, yard , ...


;; referencing http://www.bipm.org/en/publications/si-brochure/
;; and http://physics.nist.gov/Pubs/SP811/contents.html
    
  
#| Issue: Naming of units within interactive programs

This program system will endeavor to adopt a consistent syntax for
measurement names, within the context of this program system.

Consulting the SI Brochure[1], all of the seven SI base units[1] are
defined, each, around a specific measurement domain, denoted by a type
of base quantity.

Each type of base quantity is assigned with one or more conventional
symbols, such as for variables denoting of such a quantity. The
applications may vary, for those conventional symbols for quantities
of the base unit -- for example, "l" being applied commonly for
measure of linear dimesion, and "r" applied commonly for radial
magnitude, each of the previous  denoted as a conventional symbol for
a variable denoting of a measure of length. This application system
will not make a formal defintion of those conventional symbols for
variables denoting of quantity. 

Each type of measurement base quantity corresponds to exactly one
measurement base unit. 

Each base unit is denoted with a unit name -- for instance, "second"
-- and one or more symbols denoting of the measurement unit -- for
instance "s". 

For purpose of referene within software applications, this program
system will endeavor to define a normative syntax for measurement
units, consistent with common practice. In that context, it is a 
requirement of this system that all measurement units defined by this
system may be referenced with a symbolic name comprised of printing
characters within the ASCII character set.

In common practice, some measurement units are denoted with non-ASCII
characters and qualities of typesetting -- including subscript
characters and greek letter characters -- some of which are available
however, within the Unicode Code Set. 

This program system will endeavor to define the following values for
each type of measurment unit (slots of class, MEASUREMENT-UNIT)

* QUANTITY-NAME - Name of base quantity, e.g "time, duration"

* PRINT-LABEL - A singular, verbose, printable name for the measurement
  unit, wtihout symbolic characters e.g. "meter", "newton meter"

* PRINT-NAME - A singular, printable name for symbolic
  representation of the measurment unit in conventional syntax -- in 
  which instance, this system assumes that the Lisp implementation
  implements the Unicode Code Set -- e.g. "m", "°", "Ω", or "Lᵥ"

* NAME - A Lisp symbolic name, interned within the keyword package, for
  application within the source code of Lisp programs -- e.g. :m :deg
  (angular) :ohm, and :lux respectively. For purpose of this
  definition, a number of standard practices will be defined of this
  system:

   * ASCII characters shall be represented as ASCII characters to be
     interpreted in "readtable case"

     e.g "m" => :m, "mol" => :mol, "K" => :k

   * Superscirpt characters shall be prefixed with a caret "^"  and
     represented without typesetting 

     e.g. "cubic meter" = :m^3

   * For measurement units defined by SI[1] with subscript characters,
     a corresponding symbolic name shall be sought of the NIST
     specification[2] 

     e.g symbolic name "Lᵥ" in SI[1], interpreted rather as "lx" in
     NIST[2] and therefore => :lx


   * For measurement units whose conventional symbolic name denotes a
     ratio among measurement units, the character denoting the ratio
     shall be retained

     e.g "kg/m³" => :kg/m^3

   
   * For measurement units whose conventional symbolic name includes a
     subscript character, the character shall be prefixed by an
     underscore "_" and rendered without typesetting

     e.g. "mₑ" => :m_e

   * For the special instance of the radian or steradian, this program
     system shall retain the respective, conventional symbolic name --
     respectively "rad" or "sr" i.e. :rad or :sr in Lisp symoblic
     syntax. This decision is correlated with a note: That in a
     conventional shorthand practice for mathematical equations,
     namely the symbolic name of the radain may be omitted from 
     mathematical equations; radians, as a measurement unit, amy
     contextually be differentiated from degrees, as a measurment
     unit, in that a mesurement denoting a degree measure would be
     suffixed with the printed name "°"

   * For the special instance of "rad" as a ratio of Grays, this
     proram system shall use the symbolic name "rd" i.e :rd

   * For each of the special instances of a measure in units of 
     "degree" (angular measure) or "degree Celsius" (thermodynamic
     temperature). the non-ASCII character "°" shall be transposed to
     a short hand letter form - respectively, :deg, :deg-c

   * For the non-SI unit of measurement "degree Farenheit", similarly
     the shothand form :deg-f shall be applied

   * For the SI unit of measurement "degree Kelvin", the shorthand
     form :k shall be applied, as transposing the symbolic unit
     identity "K"

   * For measures of plane angle in units of minutes or of seconds,
     this system shall use the letter forms respectively
     :min_angular and :sec_angular, with printed representation
     respectively "'" and '"'

   * For the special instance of the measurement unit, angstrom "Å",
     this system shall use the conventional name of the measurement
     unit, without diacritic marks, i.e. :angstrom

In defining those policies for Lisp symoblic names for measurement
units, this sytem endeavors to present a convenient balance between
needs for symbolic uniqueness and expressive clarity.

In denoting formal printable labels for measurement units, this system
shall defer firstly to to the SI brochure[1] (noting, namely, Tables 1
through 4, table 6, etc) excepting those centimetere-gram-second (CGS)
units of measure as denoted "Unacceptable" by the NIST guide[2]
sections 5.3.1 and 5.3.2

For printed names utilizing special typographic characters in
superscript or subscript notations, this system will apply the Unicode
character equivalent of the respective supercript or subscript, when
available.  

It should be noted that an alternate syntax may be developed in
extending of  MathML syntax, such that may be integrated with
electronic publishing systems. However, until if this sytsem may have
reached an extent of development as to provide an integration for
MathML with a corresponding desktop interface and office document
publishing system, this system shall instead apply a typographic
shorthand of those letters' special typographic forms, so far as
available withinin the Unicode code set.

[1] BIPM. SI Brochure: The International System of Units (SI) [8th
    edition, 2006; updated in 2014]
    available: http://www.bipm.org/en/publications/si-brochure/ 

[2] NIST. Guide for the Use of the International System of Units (SI)
    http://www.nist.gov/pml/pubs/sp811/index.cfm 

|#

(defclass measurement-domain ()
  ((quantity
    ;; FIXME: #I18N - localize the base-quantity name during class initailization
    :type simple-string
    :initarg :quantity
    :reader :measurement-domain-quantity
   (unit
    ;; symbol for measurement base unit 
    ;;
    ;; applied as a canonical representation of a measurement unit,
    ;; within program source code
    ;;
    ;; syntax: 
    ;;
    ;; 1. when the standard symbol for the measurement unit may be
    ;;    expressed, in its entirety, as an ANSI CLtL 'base-char',
    ;;    then the standard measurement symbol, transposed to reader
    ;;    case
    ;;
    ;; 2. when the standard symbol for the measurement unit is
    ;;    expressed as a greek letter -- e.g. "upper case" omega for
    ;;    measure of electrical resistance -- ... ?
    ;;
    ;; For instances in which a measurement unit is defined with a
    ;; standard symbol outside the range of ASCII character text
    ;; (i.e. ANSI CLtL 'base-char' characters) -- such as lumious
    ;; intesnity (I subscript V) or electric resistance 
    :type symbol
    :initarg :unit
    :reader measurement-domain-unit)
   (unit-label
    ;; human-readable name of a measurement unit, per [...]
    ;; e.g. "metre" (FIXME: #I18N...)
    ;;
    ;; notes:
    ;; * some measurement units have more than one standard
    ;;    symbol, e.g. "wavenumber" and "mass concentration"
    :type simple-string
    :initarg :unit-label
    :reader measurement-domain-unit-label)
   (unit-symbol-label
    :type sipmle-string
    :initarg :unit-symbol-label
    :reader measurement-domain-unit-symbol-label
    )))

(defclass measurement-class (measurement-domain standard-class)
  ())



#+NIL ;; utility form - eval in REPL, transpose to source file
(dolist (spec '((meter "length" "metre" "m" :m)
                (kilogram "mass" "kilogram" "kg" :kg)
                (second "time, duration" "second" "s" :s)
                (ampere "electric current" "ampere" "A" :a)
                (kelvin "thermodyamic temperature" "kelvin" "K" :k)
                (mole "amount of substance" "mole" "mol" :mol)
                (candela "luminous intensity" "candela" "cd" :cd)))
  (destructuring-bind 
        (class quantity label s-label s) spec
    
    (print `(defclass ,class ()
              ()
              (:unit . ,s)
              (:unit-symbol-label . ,s-label)
              (:unit-label . ,label)
              (:quantity . ,quantity)
              (:class measurement-class))
           t)))


(defmacro register-measurement-class (name unit)
  (with-gensym (class)
  `(let ((,class (defclass ,name ()
                   ()
                   (:unit . ,unit)
                   ...
                   )))
     )))

(defun find-unit-using-symbol (s)
  (declare (type symbol s)
           (values measurement-class))
  (

(defgeneric convert (scalar unit)
  (:method (scalar (unit symbol))
    (convert scalar (find-unit-using-symbol unit))
    ))
                   
  

(DEFCLASS METER NIL NIL (:UNIT . :M) (:UNIT-SYMBOL-LABEL . "m")
          (:UNIT-LABEL . "metre") (:QUANTITY . "length")
          (:CLASS MEASUREMENT-CLASS)) 

(DEFCLASS KILOGRAM NIL NIL (:UNIT . :KG) (:UNIT-SYMBOL-LABEL . "kg")
          (:UNIT-LABEL . "kilogram") (:QUANTITY . "mass")
          (:CLASS MEASUREMENT-CLASS)) 

(DEFCLASS SECOND NIL NIL (:UNIT . :S) (:UNIT-SYMBOL-LABEL . "s")
          (:UNIT-LABEL . "second") (:QUANTITY . "time, duration")
          (:CLASS MEASUREMENT-CLASS)) 

(DEFCLASS AMPERE NIL NIL (:UNIT . :A) (:UNIT-SYMBOL-LABEL . "A")
          (:UNIT-LABEL . "ampere") (:QUANTITY . "electric current")
          (:CLASS MEASUREMENT-CLASS)) 

(DEFCLASS KELVIN NIL NIL (:UNIT . :K) (:UNIT-SYMBOL-LABEL . "K")
          (:UNIT-LABEL . "kelvin") (:QUANTITY . "thermodyamic temperature")
          (:CLASS MEASUREMENT-CLASS)) 

(DEFCLASS MOLE NIL NIL (:UNIT . :MOL) (:UNIT-SYMBOL-LABEL . "mol")
          (:UNIT-LABEL . "mole") (:QUANTITY . "amount of substance")
          (:CLASS MEASUREMENT-CLASS)) 

(DEFCLASS CANDELA NIL NIL (:UNIT . :CD) (:UNIT-SYMBOL-LABEL . "cd")
          (:UNIT-LABEL . "candela") (:QUANTITY . "luminous intensity")
          (:CLASS MEASUREMENT-CLASS)) 

;; SEE ALSO...
;; http://goldbook.iupac.org/list_math.html
;; esp. http://goldbook.iupac.org/list_goldbook_quantities_defs_A.html


;; referencing http://www.hytime.org/materials/hi2mdhyt.sgm
;;
;; notation names and public identifiers, for 'standard measurement unit'
;; (SMU) definitions...
;;
;; from the HyTime Granule Definition Notation module ...
;;
;; of "ISO/IEC 10744:1997", i.e. Hypermedia/Time-based Structuring
;; Language (HyTime)
;;
;; gQuantum "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"
;; SIsecond "ISO/IEC 10744:1997//NOTATION Systeme International second//EN"
;; SImeter "ISO/IEC 10744:1997//NOTATION Systeme International meter//EN"
;; virTime "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"
;; virSpace "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"
;; SIkg "ISO/IEC 10744:1997//NOTATION Systeme International kilogram//EN"
;; SIcd ""ISO/IEC 10744:1997//NOTATION Systeme International candela//EN"
;; SIampere "ISO/IEC 10744:1997//NOTATION Systeme International ampere//EN"
;; SImole "ISO/IEC 10744:1997//NOTATION Systeme International  mole//EN"
;; SIradian "ISO/IEC 10744:1997//NOTATION Systeme International radian//EN"
;; SIsr "ISO/IEC 10744:1997//NOTATION Systeme International steradian//EN"
;;
;; Issue: The gQuantum, virTime and virSpace notations share the same
;; public idenifier, but may be differentiated by their respective
;; notation names. Though practically useful, however those
;; measurement units are not standardized onto SI
;;
;; see also: 
;; http://crism.maden.org/consulting/pub/hytime/meas.html (1992)
;; http://www.is-thought.co.uk/schedule.htm




(defclass scalar ()
  ((magnitude
    :type real
    :accessor scalar-magnitude)
   (exponent
    :type integer)
   ))
   


#| a short reference onto the unit-formulas system

  unit-formulas::*units* => hash table (EQL) [internal]
    key: STRING. value: UNIT 

  REDUCE-UNIT  - when provided with a symoblic unit name, accesses unit-formulas::*units* to return a UNIT object; errs if no unit is defined for the unit name
 
  UNIT-FORMULAS::UNITS-OF - ? [accessor] [internal] 


  QUERY-UNIT - "Return a plist of unit value and unit exponents", with consing
  e.g (query-unit (reduce-unit :degree))

  (CONVERT-UNIT


|#
