Igneous-Math - A Mathematical Object System in Common Lisp
==========================================================

## Overview

**Availability:** [git@github.com:MetaCommunity/igneous-math.git][igneous-math]

**Dependencies:**

* [mci-cltl-utils][mci-cltl-utils]
* [closer-mop][c2mop]
* [bordeaux-threads][bordeaux-threads]

**Supported Implementations**

This program system is being developed primarily with
[Steel Bank Common Lisp (SBCL)](http://www.sbcl.org). Though some
effort is made to ensure portability with other Common Lisp
implementations, however SBCL is effectively the _reference platform_
in development of this program system.

**License:** [Eclipse Public License 1.0][license]

### Features (Current Edition)

* _**Object model for measurements**_, extending of the Common Lisp Object System (CLOS)
* _**Mathematical operations overloaded**_ within CLOS methods and optimized method forms, extending of the Common Lisp MetaObject Protocol (MOP)
* _**Coercion for floating-point values**_, on input, into rational scalar objects, each comprised of an integer magnitude and an integer prefix value

### Features (Planned)

* Implementation of an object model for coordinate systems and coordinate syntaxes, independent of the Common Lisp Interface Manager (CLIM)
    * Euclidean Coordinate Plane
	* Polar Coordinate Plane
* Implementation of spatial object systems
	* 3 Space (Euclidean)
	* Spherical Space
* Graph model for visual coordinate system presentation, integrating with CLIM
* Object model for vector mathematics in common lisp (planar and
  spatial object systems) 
* Comprehensive integration with standard systems of measurement,
  referencing the Systeme Inernationale (BIPM) and NIST guidelines
* Automatic adjustment of measurement values onto the decimal prefix
  system, for standard measurement units 
* Automatic adjustment of measurement values onto the binary+decimal
  (2^10) prefix system, for quantities of digital information
  [][#jedec:jesd-100b.01][][#wikipedia:byte] preferring the JEDEC
  convention for prefixes "K", "M", and "G" for units of bits, "b",
  and bytes, "B"  with user option for selection of a consistent IEC
  prefix system [][#wikipedia:byte]

### Features (Planned) (Tentative)

* Implementation of an object model for formulas in CLOS -- referencing the KR system in the [Garnet][garnet] codebase
* Integration with machine-specific numeric operations
* Applications towards modeling and analysis of principles developed
  in the electrical sciences, separately onto domains of alternating
  current and direct current 
* Integration with formal reference texts, eBook and print editions

## Reference (Partial)

### Measurement Concepts

### Measurement Domains

#### Base Measurement Domains

The Systeme International (SI), published by the _Bureau International
des Poids et Mesures_ (BIPM), defines seven base measurement units,
of which any additional measurement units may be directly or
indirectly derived[BIPM][#BIPM].

* *length*
* *mass*
* *time, duration*
* *electric current*
* *thermodynamic temperature*
* *amount of substance*
* *luminous intensity*

Each of the fundamental _measurement domains_ is defined with a
corresponding _measurement base unit_.

* **Length:** meter (alternately, metre)
* **Mass:**  kilogram
* **Time, Duration:** second
* **Electric Current:** ampere
* **Thermodynamic Temperature:** kelvin
* **Amount of Substance**: mole
* **Luminous Intensity**: candela

This program system represents those respective measurement domains
and their respective base measure units as follows:

* Domain `math:length`, base `math:meter`
* Domain `math:mass`, base `math:kilogram`
* Domain `math:time`, base `math:second`
* Domain `math:electrical-current`, base `math:ampere`
* Domain `math:temperature`, base `math:kelvin`
* Domain `math:amount-substance`, base `math:mole`
* Domain `math:luminous-intensity`, base `math:candela`


In the object system defined of the
[Igneous-Math source tree][[igneous-math], effectively a
_`math:measurement` class_ is an instance of a
_`math:measurement-class`_, such as `math:meter`. Each
_`math:measurement-class`_, in turn -- such as `math:length` -- is an
instance of a  _`math:measurement-domain` class_.

>  (use-package '#:math)
>  => T
>
>  (defparameter *m*  (make-measurement 1 :|m|))
>  => *M*
>
> *M*
> => #<METER 1 m {10083AF7A3}>
>
>  (class-of *m*)
>  => #<LENGTH METER>
>
>  (class-of (class-of *M*))
> => #<MEASUREMENT-DOMAIN LENGTH>


**Note:** This system develops a concept of a _measurement domain_, in
some regards independent of the referenced standards for measurement
systems. Essentially, this system defines a _measurement domain_ as
an _object_ effectively shared between _measurement units_ having a
same _base measurement_, though possibly having a differeing
_dimensionality_ onto the _base measurement_. In many regards, this
definition is developed towards a sense of _computational
convenience_.

For example: Units of _length_, of _area_, and of _volume_ all derive
from the same essential domain of _length_ -- respectively, being
of dimensionality _1_, _2_, and _3_ onto the base measurement of 
_length_. In a mathematical operation in which any two values of
_length_ are _multiplied_, for example, the result is a measure of
_area_ -- essentially, a measurement of a dimensionality equivalent to
the sum of the degrees of the dimensionalities of the provided
values, in the geometrically expansive mathematical operation,
_multiplication_.

Similarly, dimensions of _duration_ and of _frequency_ may both be
derived from a base unit measurement of _time and duration_ -- a
_duration_ measurement being of a dimensionality _1_ onto _time and
duration_ and a_frequency_ measurement being of a dimensionality
_-1_. 

### Derived Units of Mesurement

In standards published by the BIPM and in standards published by the
NIST, standard base units are defined and subsequently extended with
exacting formulas, each describing the mathematical nature of a
conventional _derived unit of mesurement_. [BIPM][#BIPM][NIST][#NIST]

#### Linearly Derived Units of Measurement

Axiom: _Measurement units_ defined within an equivalent _measurment
domain_ and of an equivalent _dimensionality_ may be scaled, linearly
-- each, per a ratio defining how many units of the _base measure_ 
exist within one unit of the _derived measure_, or reciprocally, how
many units of the _derived measure_ exist within the _base mesaure_.

_See, for example, `MATH:FOOT` and `MATH:METER`_

**Implementation Notes:**

* `math::measurement-base-factor`
* `math::measurement-base-factor-exponent`


#### Formulaiclly Derived Units of Measurement

_To do: Still developing the architecture for derived units_

**Concepts:**

* Algebraic conversions between measurement units in non-linear
  derivations
* Caching and quering for known measurement unit conversion formulas
* Application of measurement unit conversion formulas, within
  mathematical operations onto measurement values


**Previous Documentation (2) :**

Referring to [BIPM][#BIPM], p. 118 (English Edition PDF p. 26) a
number of additional _measurement domains_ are defined within a single
table, each denotedper an essential type of _quantity_. Within a
domain of applications of electrical systems, the folloiwng units are
denoted of that table:

* Volt (V)
    * Domain: _Electrical power difference, electromotive force_
	* Relation in terms of SI derived units: `W/A`
	* Relation in terms of SI base units: `m^2 kg s^-3 A^-1`
	* Common Applications include _Ohm's Law_ -- in a classic form,
	  `I=V/R`
	* ....

**Previous Documentation (1) :**

The measurement unit, _ohm_, as a standard unit for measurement of
quantity of _electrial resistance_, is defined as a derived unit with
formulas:

    1 ohm = 1 m^2 kg s^-3 A^-2	[1] p. 111	
    1 ohm = 1 V / 1 A		[1] p. 118

Sidebar: Ideally, a mathematical system should be able to determine
what units of measurement have resulted from a single formula. For
instance, if a unit of _length_ is multiplied by a unit of _length_,
then intuitively, the unit of the resulting multiple is a unit of
_area_.  If the resultimg unit of _area_ would be multiplied, then, by
another unit of _length_, then the resulting unit would be a unit of
_cubic volume_, intuitively. Alternatley, if a unit of _area_ would be
divided by a unit of _length_, then the resulting unit would be a unit
of _length_.

Essentially, this describes a manner of _unit algebra_,  specifically
as with regards to _measurement units_ as _objects_ having some
qualities like real numbers.

In a prototypical regards, assuming a symbolic representation of unit
names in Common Lisp:

    (unit :|ohm|) = (unit (:|m| 2) :|kg| (:|s| -3) (:A -2))
	              = (unit :V (:A -1))


Similarly, the measurement unit, _volt_, as a standard unit for
measurement of quantity of _electromotive force_ or _difference in
electrical potential_, is defined with formulas

    1 volt = 1 m^2 kg s^-3 A^-1	[1] p. 118
    1 volt = 1 W / 1 A		[1] p. 118

The measurement unit, _watt_, as a standard unit for measurement of
quantity of _power_ or _radiant flux_:

    1 watt = 1 m^2 kg s^-3		[1] p. 118
    1 watt = 1 J / 1 S		[1] p. 118

The measurement unit, _joule_, as a standard unit for measurement of
quantity of _work_, _energy_, or _amount of heat_:

    1 joule = 1 m^2 kg s^-2	[1] p. 118
    1 joule = 1 N * 1 M		[1] p. 118

The measurement unit, _newton_, as a standard unit for measurement of
quantity of _force_: 

    1 newton = 1 m kg s^-2		[1] p. 118


    [1] http://www.bipm.org/en/publications/si-brochure/


### Measurement Unit Conversions within Mathematical Operations

The Igenous-Math system defines a set of _overloaded_ mathematical
operations, such that extend of an implementation's own optmized
mathematical operations. _(xref: `MATH::DEFOP`)_

For extending so much as the simple, `"+"`, `"-"`, `"*"`, and `"/"`
operations onto _measurement_ objects, some consideration must be made
as to how those respective operations would affect units of
measurement on input measurement values. 

In a diadic mathematical operation, onto two values of the same
mesaurement domain:

* For _linear_ mathematical operations `"+"` and `"-"`,  then each
  respective value may be scaled effectively to the _base
  measurement_ of the _measurment domain_ and the effective magnitudes 
  resulting of the _scale_ then effectively _summed_ or _differenced_,
  to a resulting object of the same _base measurement_ as the
  _measurement domain_, of a scalar degree appropriate to the input
  values and the mathematical operation
  
* For a _geometrically expansive_ mathematical operation, such as
  `"*"`, then ...
  
* For a _geometrically reductive_ mathematical operation, such as
  `"/"`, then ...


### Object Naming

Each type of measurement _base quantity_ corresponds to exactly one
_measurement base unit_. Each _base unit_ is denoted with a _unit
name_ -- for instance, "second" -- and one or more _symbols_ denoting
of the measurement unit -- for instance "s".

This program system defines the following values for each type of
measurement unit:

* name of base quantity, e.g "time, duration"
    * for a measurement *M*, `(object-print-name (class-of (class-of *M*)))`

* a singular, verbose, printable name for the measurement
  unit, without symbolic characters e.g. "second", "kilogram"
    * for a measurement *M*, `(object-print-name (class-of *M*))`

* a singular, printable label for symbolic representation of the
  measurement unit in conventional syntax -- in which instance, this
  system assumes that the lisp implementation implements the Unicode
  code set -- e.g. "m", "°", "ω", or "lᵥ".
    * for a measurement *M*, `(object-print-label (class-of *M*))`

* A Lisp symbolic name, interned within the keyword package, for
  application within the source code of Lisp programs -- e.g. `:m`,
  `:deg`, (angular), `:ohm`, and `:lux`.
    * for a measurement *M*, `(measurement-symbol (class-of *M*))`

Example:

>  (use-package '#:math)
>  => T
>
>  (defparameter *m*  (make-measurement 1 :|m|))
>  => *M*
>
> *M*
> => #<METER 1 m {10083AF7A3}>
>
>  (object-print-name (class-of (class-of *m*)))
>  => "length"
>
>  (object-print-name (class-of *m*))
> => "meter"
>
>  (object-print-label (class-of *m*))
> => "m"
>
>  (measurement-symbol (class-of *m*))
> => :|m|


Regarding selection of measurement symbols, a number of standard
practices will be defined of this system: 

   * ASCII characters shall be represented as ASCII characters to be
     interpreted in "readtable case"

     e.g `"m" => :m`, `"mol" => :mol`, `"K" => :k`

   * Superscirpt characters shall be prefixed with a caret "^"  and
     represented without typesetting 

     e.g. `"cubic meter" = :m^3`

   * For measurement units defined by SI[BIPM][#BIPM] with subscript characters,
     a corresponding symbolic name shall be sought of the NIST
     specification[NIST][#NIST] 

     e.g symbolic name `"Lᵥ"` in SI[BIPM][#BIPM], interpreted rather as `"lx"` in
     NIST[NIST][#NIST] and therefore `=> :lx`


   * For measurement units whose conventional symbolic name denotes a
     ratio among measurement units, the character denoting the ratio
     shall be retained

     e.g `"kg/m³" => :kg/m^3`

   
   * For measurement units whose conventional symbolic name includes a
     subscript character, the character shall be prefixed by an
     underscore "_" and rendered without typesetting

     e.g. `"mₑ" => :m_e`

   * For the special instance of the radian or steradian, this program
     system shall retain the respective, conventional symbolic name --
     respectively `"rad"` or `"sr"` i.e. `:rad` or `:sr` in Lisp symbolic
     syntax. This decision is correlated with a note: That in a
     conventional shorthand practice for mathematical equations,
     namely the symbolic name of the radian may be omitted from 
     mathematical equations; radians, as a measurement unit, may
     contextually be differentiated from degrees, as a measurement
     unit, in that a measurement denoting a degree measure would be
     suffixed with the printed name `"°"`

   * For the special instance of `"rad"` as a ratio of Grays, this
     program system shall use the symbolic name `"rd"` i.e `:rd`

   * For each of the special instances of a measure in units of 
     "degree" (angular measure) or "degree Celsius" (thermodynamic
     temperature). the non-ASCII character `"°"` shall be transposed to
     a short hand letter form - respectively, `:deg`, `:deg-c`

   * For the non-SI unit of measurement "degree Fahrenheit", similarly
     the shorthand form `:deg-f` shall be applied

   * For the SI unit of measurement "degree Kelvin", the shorthand
     form `:k` shall be applied, as transposing the symbolic unit
     identity "K"

   * For measures of plane angle in units of minutes or of seconds,
     this system shall use the letter forms respectively `:|'|` and
     `:|"|`, with printed representation respectively `"'"` and `'"'`

   * For the special instance of the measurement unit, angstrom `"Å"`,
     this system shall use the conventional name of the measurement
     unit, without diacritic marks, i.e. `:angstrom`

In defining those policies for Lisp symbolic names for measurement
units, this system endeavors to present a convenient balance between
needs for symbolic uniqueness and expressive clarity.

In denoting formal printable labels for measurement units, this system
shall defer firstly to to the SI brochure[BIPM][#BIPM] (noting, namely, Tables 1
through 4, table 6, etc) excepting those centimeter-gram-second (CGS)
units of measure as denoted "Unacceptable" by the NIST guide[NIST][#NIST]
sections 5.3.1 and 5.3.2

For printed names utilizing special typographic characters in
superscript or subscript notations, this system will apply the Unicode
character equivalent of the respective superscript or subscript, when
available.

It should be noted that an alternate syntax may be developed in
extending of  MathML syntax, such that may be integrated with
electronic publishing systems. However, until if this system may have
reached an extent of development as to provide an integration for
MathML with a corresponding desktop interface and office document
publishing system, this system shall instead apply a typographic
shorthand of those letters' special typographic forms, so far as
available within the Unicode code set.


A short illustration:

>  (use-package '#:math)
>  => T
>
>  (use-package #:utils)
>  => T
>
>  (defparameter *m*  (make-measurement 1 :|m|))
>  => *M*
>
> *M*
> => #<METER 1 m {10083AF7A3}>
>
>  (object-print-label *m*)
>  => "1 m"
>


See also: `utils:object-print-name`, `utils:object-print-label`

### Measurement Dictionary (Partial)

#### `Measurement-Domain` [Standard Class]

#### `Measurement-Class` [Standard Class]

#### `Measurement` [Standard Class]

#### `Prefix` [Standard Class]

#### `Rescale` [Standard Generic Function]

#### `Nrescale` [Standard Generic Function]

#### `Scale-SI` [Standard Generic Function]

#### `Find-Prefix` [Function]

#### `Find-Prefix=` [Function]

#### `Prefix-Of` [Function]

#### `Prefix-Degree` [Accessor]

#### `Prefix-Symbol` [Accessor]

#### `Measurement-Symbol` [Accessor]

#### `Measurement-Magnitude` [Accessor]

#### `Measurement-Degree` [Accessor]

#### `Base-Magnitude` [Function]

#### `Scalar-Magnitude` [Generic Function]

### Mathematics Dictionary (Partial)

#### `%+`, `@+`, `@+@`

#### `%-`, `@-`, `@-@`

#### `%*`, `@*`, `@*@`

#### `%/`, `@/`, `@/@`

## Measurement Units - Old Notes

The following text represents a body of notes developed during initial
prototyping of the Igneous Math system.

### Measurement Domains

_Section subsequently moved into reference documentation, previous_

#### Conventional Prefixes for Decimal Multiples of Measurements

The Systeme International defines a standard set of symbolic names
for decimal exponents of measurement values, in a range from
-24 to 24 degrees. Those decimal prefixes allow for succinct
representation of _magnitudes_ of _measurement values_, within
limits of _significant digits_.

#### Measurement Accuracy and Significant Digits

(To Do: Computation of significant digits within input values, with
corresponding limits onto significant digits for printed values)

See Also: Garcia-Santamarina, Sareta, et. al. _[Significant decimals and rounding](http://www.degruyter.com/view/j/cclm.2004.42.issue-9/cclm.2004.216/cclm.2004.216.xml)_

#### Structure of Virtual Measurement Units

In addition to the SI base units, this system may endeavor to define a
set of additional measurement domains, such as inspired by the
HyTime  standard[HyTime][#HyTime] -- specifically, such an abstract _virtual 
measurement_ domain -- as well as a measurement domain for quantities
of digital information.

The _virtual measurement_ domain is extended with base units, namely,
_virtual time_ and _virtual space_ -- applicable as for purpose of a
representation of measurable quantities within a software system
respectively, such as _PAL frame rate_ and _pixel coordinates_[MBryan][#MBryan]



### Measurement Syntax

This program system will endeavor to adopt a consistent syntax for
measurement names, within the context of this program system.

### Structure of Measurement Units

_Section subsequently moved into reference documentation, previous_

### Geometric Object Model

This system is being developed for a purpose of defining a consistent
measurement model for application in analytic geometry. In order to
ensure that this system would provide a model consistent onto
standards for conventional practice in measurement modeling, and would
implement a mathematically reliable methodology for conversion of
metric values, this system's development effectively must be informed
of standards for "best practice."

In extending of the measurement model implemented of this program
system,  a further set of components may be defined for representation
of properties of trigonometric systems as well as known practices in
analytic geometry. within the Common Lisp programming environment. 

For representation of measurement values, this system defines a single
class, SCALAR, namely for representation of quantities of known
measurement units. The class, SCALAR, is then extended with the class,
VECTOR, in a programmatic model implemented, internally, onto the
polar coordinate plane. 

For purpose of ensuring compatibility with mathematical methods
utilizing of qualities of the Cartesian/Euclidean and rectangular
coordinate planes, this system furthermore defines an object model for
representation of geometric forms, specifically within a Common Lisp
programming environment.


### TO DO

1) Define an ASDF system for this source file [DONE]

X) Define a reader macro syntax for measurement units
   also cf. Jakub Higersberger's unit-formula system

Y) Extend MEASUREMENT-CLASS with a new class,
   DERIVED-MEASUREMENT-CLASS, such that would publish an accessor
   for calculating a formula for measurement unit conversions

2) Implement YouTrack and TeamCity onto AWS (cf. Nr 4)

3) Move DEFCLASS* into the mci-cltl-utils source tree

4) Define, within YouTrack and TeamCity, such TO DO items as are
   denoted within the commentary in the DEFCLASS* macro definition 

5) Define a convenient syntax for extension of this measurement
   protocol in definition of custom measurement units

6) Continue with definition of the geometry component of this
   system, toward DEFCLASS VECTOR etc.

7) "Back track" to the TO DO items defined then in YouTrack/TeamCity

8) At some point, refine the comments in this file into a form of
   normative documentation for this program system

9) Define a desktop interface for this system

FIXME: The following documentation items were transposed from
measurement.lisp and should be edited for presentation in this
_markdown_ file

### Sidebar: Measurement Definitions in HyTime

* <http://www.is-thought.co.uk/schedule.htm>
* <http://crism.maden.org/consulting/pub/hytime/meas.html>
* <http://www.hytime.org/materials/hi2mdhyt.sgm>
    * ^ cf %hygrand

see also:

* <http://physics.nist.gov/cuu/units/>
* <http://physics.nist.gov/pubs/sp811/appenb.html>
    * ^ esp. for conversions regarding foot, mile, yard , ...


[igneous-math]: https://github.com/MetaCommunity/igneous-math
[mci-cltl-utils]: https://github.com/MetaCommunity/mci-cltl-utils
[c2mop]: http://sourceforge.net/projects/closer/
[bordeaux-threads]: http://common-lisp.net/project/bordeaux-threads/
[license]: https://github.com/MetaCommunity/igneous-math/blob/master/LICENSE
[garnet]: http://garnetlisp.sourceforge.net/



[#wikipedia:byte]: Wikipedia. _[Byte](http://en.wikipedia.org/wiki/Byte)_

[#jedec:jesd-100b.01]: JEDEC. _[JESD-100B.01. Terms, Definitions, and Letter Symbols for Microcomputers, Microprocessors, and Memory Integrated Circuits](http://www.jedec.org/standards-documents/docs/jesd-100b01)_ 

[#BIPM]: BIPM. _[SI Brochure: The International System of Units (SI)](http://www.bipm.org/en/publications/si-brochure/)_. 8th edition,
	2006; updated in 2014. 

[#NIST]: NIST. _[Guide for the Use of the International System of Units (SI)](http://www.nist.gov/pml/pubs/sp811/index.cfm)_

[#EToolbox]: _[The Engineering Toolbox. Standard Form, Scientific, and Engineering Notation)(http://www.engineeringtoolbox.com/standard-form-scientific-engineering-notation-d_1801.html)_

[#HyTime]: _[HyTime Granule Definition Notation](http://www.hytime.org/materials/hi2mdhyt.sgm)_

[#MBryan]: Martin Bryan. _[Using HyTime for Scheduling Events](http://www.is-thought.co.uk/schedule.htm)_

<!--  LocalWords:  mci cltl utils bordeaux SBCL CLOS MetaObject CLIM
 -->
<!--  LocalWords:  syntaxes Systeme BIPM NIST wikipedia des Poids et
 -->
<!--  LocalWords:  JEDEC IEC codebase eBook Mesures metre candela al
 -->
<!--  LocalWords:  Santamarina Sareta HyTime MBryan lux readtable mol
 -->
<!--  LocalWords:  Superscirpt lx radian steradian sr radians CGS AWS
 -->
<!--  LocalWords:  MathML ASDF Jakub Higersberger's accessor YouTrack
 -->
<!--  LocalWords:  TeamCity Nr DEFCLASS FIXME hygrand JESD Rescale
 -->
<!--  LocalWords:  EToolbox defparameter Nrescale
 -->
