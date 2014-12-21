Igneous-Math - A Mathematical Object System in Common Lisp
==========================================================

## Overview: Igneous-Math

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

Although this sytem endeavors to implement ANSI Common Lisp, as for
purpose of portability, however in regards to extensions of the
Metobject Protocol, some implemetation-specific features might
vary.

Pending further development of Igneous-Math onto the specified
_reference platform_, the Igneous-Math codebase may be revised for 
portability onto other Common Lisp implementations.

**License:** [Eclipse Public License 1.0][license]

### Design, Context, and Application

This system has been designed initially as an academic
exercise. It is hoped that this system may be developed
towards a level of application quality, such that this system may be
considered for commercial applications. To this time, the Igneous-Math
software system has been designed towards a concept of a manner of 
_semantic normalcy_, although not as if to make a sacrifice of
_mathematical accuracy,_ in defining the semantics of the
implementation.

### Features (Ig<sup>1</sup><sub>m</sub>)

* _**Object model for measurements**_. Extending of the Common Lisp 
  Object System (CLOS), Igneous-Math defines a model for measurement
  domains, measurement classes, and expressions of measurement units.
* _**Mathematical operations**_. Igneous-Math defines a set of generic
  functions and methods as interfaces onto the set of numeric 
  functions defined in CLtL2. These generic functions and methods are
  specialized for _monadic_, _diadic_, and _variadic_ application --
  in extension of the Common Lisp metaobject system -- moreover
  specialized onto individual classes of _numeric field_ as defined in
  CLtL2.
* _**Rational coercion for floating-point values**. On input, a
  _floating point value_ may be converted into a rational value,
  comprised of an integer and an exponent, such that would represent a
  value _scaled_ of the original input value. Subsequent mathematical
  operations may then be applied as onto the rational equivalent of
  the input value, namely as to minimize inaccuracies attendant with
  point numeric implementations, in individual floating point 
  errors and in _feedback errors_ (see also: Hamming, Richard
  W. _Numerical Methods for   Scientists and Engineers_. 2
  ed. Dover. 1973)

The implementation of _rational coercion_ is effectively integrated
with the _measurements model_ in Ig<sup>1</sup><sub>m</sub>

### Features (Planned)

* Implementation of forms for vector methamtics and linear analysis
    * _Scalar_ value type, corresponding with the measurements module
    * _Vector_ value type, with corresponding numerical operations
    * _Matrix_ values and corresponding mathematical operations
* Implementation of an object model for coordinate systems and
  coordinate syntaxes, independent of the Common Lisp Interface
  Manager (CLIM) 
    * Euclidean Coordinate Plane
        * Extensionally, an implementation of a complex coordinate
          plane, graphically analogous to the Euclidean coordinate
          plane
	* Polar Coordinate Plane
* Presentation of _phasor diagrams_, in applying the modules for
  vector mathematics and for polar coordinate planes
* Implementation of spatial object systems
	* 3 Space (Euclidean)
	* Spherical Space
* Object model for vector mathematics in spatial object systems
* Graph model for visual coordinate system presentation, integrating
  with CLIM (see also: McCLIM _Scigraph_ application)
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

* Implementation of an object model for formulas in CLOS --
  referencing the KR system in the [Garnet][garnet] codebase
* Integration with machine-specific numeric operations (e.g SSE2)
* Applications towards modeling and analysis of principles developed
  in the electrical sciences, separately onto domains of alternating
  current and direct current, and onto a frequency domain for analysis
  of signal-processing applications -- ideally, to be integrated wth a
  system for schematic capture and block-level circuit modeling, onto
  McCLIM.
* Integration with formal reference texts, in eBook and print editions


## Overview: Ig<sup>2</sup><sub>m</sub>

Ig<sup>2</sup><sub>m</sub> is the _second major version_ of
[[igneous-math][igneous-math]]. Essentially,
Ig<sup>2</sup><sub>m</sub> represents a restructuring and partial
refactoring of the Ig<sup>1</sup><sub>m</sub> _source tree_, with
corresponding development of individual _modules_ begun originally in
development of Ig<sup>1</sup><sub>m</sub>.

Development in Ig<sup>2</sup><sub>m</sub> will be focused about the
following concepts:

* Extension of the base measurements module, for _geometric units of
  measurement_ and _compound units of measurement_
* _Normalization_ and _conversion_ of measurement units, within
  _overloaded_ mathematical operations
* Definition of seperate _reader macros_ for each of _unit expresions_
  and _measurement_ values containing _unit expressions_, e.g.
  `#{3.0E+08 <m <s -1>>}`

The sections of this outline, immediately following, are developed
for application about the design of the first of those three modules.

## Changes in Ig<sup>2</sup><sub>m</sub>

### Relation of Measurement Domain and Measurement Class

In Ig<sup>1</sup><sub>m</sub>, each of the seven fundamental base
units of the Systeme International was impelemented such as with the
following example: The class `METER` would have as its metaclass, the
class `LENGTH`. The class `LENGTH` would itself represent a
_measurement domain_, while likewise being defined as a _metaclass_.

During the development of classes for _dervied units_ in
Ig<sup>2</sup><sub>m</sub>, that feature of the design of
Ig<sup>1</sup><sub>m</sub> has been reconsidered.

In Ig<sup>2</sup><sub>m</sub>, the following additional classes are
defined, as for differentiation of measurement syntaxes defined to
specific types of measurement unit:

* `LINEAR-MEASUREMENT-CLASS`
* `GEOMETRIC-MEASUREMENT-CLASS`
* `COMPOUND-MEASUREMENT-CLASS`

Furthremore, the following two classes are defined as for
differentiation between _derived units_ and _base units_

* `DERIVED-MEASUREMENT-CLASS`
* `BASE-MEASUREMENT-CLASS`

Effectively, every `BASE-MEASUREMENT-CLASS` is likewise a
`LINEAR-MEASUREMENT-CLASS`, though certainly not every
`LINEAR-MEASUREMENT-CLASS` is likewise a
`BASE-MEASUREMENT-CLASS`. Semantically, however, the concepts of
_Derived units and base units_ is orthogonal to the concept of _linear
and other types of measurement class_ as developed in
Ig<sup>2</sup><sub>m</sub>.


As a further feature of the design of Ig<sup>1</sup><sub>m</sub>,
originally: Considering that a _measurement unit_ would have a
_measurement domain_ as its metaclass, then the _measurement domain_
of a _measurement unit_ may be calculated by computing the class of
the _measurement unit_'s representative class. To retain this feature
onto the design of Ig<sup>2</sup><sub>m</sub>, however, there must be
a differention made somewhere semantically _between_ a _measurement
domain_ and a _measurement class_, as to differentiate the
_measurement class_ for whether it is a _base measurement unit_ or a
_derived measurement unit_.

During development of Ig<sup>2</sup><sub>m</sub>, a _prototype_ was
defined wherein a _measurement domain_ would be extended (by way of
subclass) by two additional _measurement domains_, one solely
representative of a _base_ or _fundamental_ unit of measure for the
domain, and the other for representing all types of _derived
measurement_ deriving from the _base_ or _fundamental measure_ of the
associated _measurement domain_. Although this seemed to present a
novel and convenient solution to the concern denoted of the previous
paragraph, but  it effectively "breaks" the procedures developed in
Ig<sup>1</sup><sub>m</sub> for indexing of measurement
domains. Therefore, this prototype must be reconsidered for its
essential meaning  and relevance, in its own regards and as it may
serve as a reflection of the design of Ig<sup>1</sup><sub>m</sub>.

Continuing with the previous example:

* Semantically, _Meter_ is a measurement of _Length_
* Programmatically, in Ig<sup>1</sup><sub>m</sub>, the class `METER`
  has as its metaclass, the class `LENGTH`. The class `LENGTH` has as
  its metaclass, the class `MEASUREMENT-DOMAIN`. Therefore, the
  `METER` class is a `LENGTH` class, and `LENGTH` is a
  `MEASUREMENT-DOMAIN`. Furthermore, an instance of type `METER` would
  be an object of type `MEASUREMENT`. 
* In Ig<sup>2</sup><sub>m</sub>, initially it was considered that the
  class `METER` must be redefined such that it  would also _be_ bot of
  a `BASE-MEASUREMENT-CLASS` and a `LINEAR-MEASUREMENT-CLASS`,
  itself. Both classes being orthogonal to the metaclass of `METER` --  
  i.e `LENGTH` -- it would pose some difficulty to _apply_ those
  classes in the exiting Ig<sup>1</sup><sub>m</sub> measurement model,
  as if to extend onto `LENGTH`


### Meaning and Application of "Magnitude" and "Value" in Measurements

During the development of Ig<sup>2</sup><sub>m</sub>, a certain issue
of _code alignment_ was observed, with regards to the following three
functions, such that were developed originally in
Ig<sup>1</sup><sub>m</sub>.

* Function `SCALAR-MAGNITUDE`
* Function `BASE-MAGNITUDE`
* Function `MESAUREMENT-MAGNITUDE`

Correspondingly, this represents an issue of an essential concern of
_conceptual alignment_ as to the naming, meaning, and application of
each of those respective functions.

#### Computational Magnitude

In a computational sense, the _magnitude_ of a value would be
calculated seperate to the _exponential degree_ of the value - such
as, with or without an overlayed _measurement prefix_ model.

As in how these functions have been developed in
Ig<sup>1</sup><sub>m</sub>: Perhaps the term "magnitde" has ben
applied too broadly, within Ig<sup>1</sup><sub>m</sub>, as if the term
was synonymous with "numeric value".

#### Relevance of Concepts "Magnitude" and "Degree", as Developed in Ig<sup>1</sup><sub>m</sub>

_Ed. Note: In an orthogonal sense, there may seem to be a further
conceputal ambiguity of these concepts as implemented, but in an
ambiguity towards concepts of 'magnitude' and 'degree' within vector 
arithmetic. Presently, that ambiguity will not be developed of 
this documentation._ 


Concerning the relevance of concepts _"Magnitude"_ and _"Degree"_, as
in how those concepts have been developed and applied in the design of
the measurements model in Ig<sup>1</sup><sub>m</sub> -- as in order to
endeavor to _disambiguate_ the concepts as _intended_ and the concepts
as _implemented_ of the design of each of those three functions
denoted in the previous outline:

The _measurement prefix_ model developed of Ig<sup>1</sup><sub>m</sub>
represents one of the primary concepts underlying the development of
Ig<sup>1</sup><sub>m</sub>. Essentially, the _mesaurement prefix_
model was to the origins of the _measurements_ design in
Ig<sup>1</sup><sub>m</sub> -- in which, a _measurement_ may be
evaluated seperately as either _numeric value_ or as a _scalar value_
of distinct _magnitude_ and _degree_.

Simply, the _numeric value_ of a _measurement_ would be drived from
its _scalar properties_ of _scalar magnitude_ and _scalar degree_, as
for example: 

> Magnitude: 5
> Degree: 2
> Base: 10
> => numeric value: 5 * 10^2 = 500

Although in this model, there are essentially three fields required
for recording each numeric value, and so it may seem to be either
_redundant_ or _excessively complex_, but there may be some
computational advantages developed of such an elemental encoding for a
numeric value. For instance:

* The _base_ value need not be encoded into the numeric value itself,
  but rather may be derived of a _metaclass_ of the numeric
  value. Thus, only the _magnitude_ and _degree_ would need to be
  recorded for each respective numeric value.

* If a prefix measurement system would be applied onto the numeric
  value, then it would be a relatively straightforard process to
  compute a nearest _prefix_ for the numeric value, as based on the
  encoded _degree_ of the numeric value.

* With regards to _rational scaling_ of decimal numeric values, this
  manner of rational, elemental encoding for numeric values
  effectively serves to allow for computations to be completed all
  with _rational intermediary values_, even for _decimal input
  values_. Thus, this design may  effectively  serve to minimize
  the numer of instances of possible _feedback errors_ as may occur
  due to _floating point contagion_ during calculatin of _intermediary
  values_ within the computational system.

In a sense of the design of Igneous-Math as a mathematical system,
the concept of a _magnitude_ -- namely, in which a _magnitude_ would
represent a _scalar property_ of a _numeric value_, as a _magnitude_
being a _rational value_ that would be _distinct to_ but not altogether
_seperate from_ an integer _scalar decimal exponent property_ i.e
_scalar degree property_ for the numeric value -- essentially, that
was thought to be one of the stronger conceputal _"selling points"_
for the design of Igneous-Math - although it may be difficult to
document, in any ways succinctly.

#### Ambiguity of Concepts of "Magnitude", as Developed in Ig<sup>1</sup><sub>m</sub>

Notably, in the design of Ig<sup>1</sup><sub>m</sub>, the concept of 
_magnitude_ -- as in sense developed of the previous paragraph -- the
concept, in its applications, had become essentially _confounded_ with
a broader concept of _numeric value_ -- thus, becoming to something of
a sense of ambiguity, as in how the concept is applied in the
_definition_ of each of the functions 
* `SCALAR-MAGNITUDE`
* `BASE-MAGNITUDE`
* `MEASUREMENT-MAGNITUDE`
...and in the concept is applied in those functions' respective
_applications_, within Igneous-Math (Ig<sup>1</sup><sub>m</sub>)

The same _conceptual ambiguity_ may become apparent, when the system
for  _measurement conversion_ -- as develoepd of
Ig<sup>1</sup><sub>m</sub> -- would be analyzed, as to consider both
of the following concerns:

1. How the _measurement conversion_ features of Igneous-Math were
   designed in Ig<sup>1</sup><sub>m</sub>. Notably, this feature has
   not been well documented of the design of
   Ig<sup>1</sup><sub>m</sub>, but insofar as the mechanics of this
   feature are apparent of the source code, then it may be discovered
   of the design of Ig<sup>1</sup><sub>m</sub> -- albeit, not until
   after some relatively _time consuming_ analysis.
   
2. How the design of Ig<sup>1</sup><sub>m</sub> may be _refactored_,
   in any regards, as ideally to introduce some more of a
   computational efficiency in measurement conversions, within the
   Igneous-Math system -- such as, specifically, towards making a
   further application of a particular computational _"short cut"_ for
   conversion of a measurement values onto the _base measure_ of a
   measurement's _measurement domain_ -- such that the _"short cut"_
   would need not apply the broader, linear _measuremnt conversion_
   framework, as also developed in Ig<sup>1</sup><sub>m</sub> _...and 
   also not presently well documented of this computational system,
   with the author's sinceremost apology._

Considering those as being two issues of analytical concern, the
second of those two issues may serve to lend way towards an actual
refactoring of the design of the same system.

With limited further apology, the author will not endeavor to comment
at great length, towards any broader view of how the same
computational system is evolving in its development in
Ig<sup>2</sup><sub>m</sub>.

## Application of a Concept of Numeric Value, in Ig<sup>1</sup><sub>m</sub>

If the author may denote a second bunch of concerns, succinctly: In
the development of Ig<sup>2</sup><sub>m</sub>, there have also been
some concerns observed, as with regards to the system for _measurement 
conversion_ -- such as developed in Ig<sup>1</sup><sub>m</sub> -- as
when that system is applied across the disjunction of the definitions
of the measurement units _kilogram_ and _gram_. This issue has been
observed within _functional testing_, during the development of
Ig<sup>2</sup><sub>m</sub>.

Simply, whereas the measurement unit, _kilogram_, is defined as the
_base unit of measure_ for quantities of _mass_, then the measurement
unit, _gram_ may be denoted as a _derived unit_. Though it is the unit
of _zero degree_ within the _base measure_ for _mass_, but it is not
the standard _base unit of measure_ for quantities of _mass_, such as
standardized in the _Systeme International_ and such as would be
applied in any number of formulas entailing of quantities of _mass_,
as within calculations onto the classical mechanics.

Although there was an implementation of a measurement unit _kilogram_,
in computational system comprised of Ig<sup>1</sup><sub>m</sub>, but
the corresponding measurement unit, _gram_, had not been implemented
in the same system. Certainly, Ig<sup>1</sup><sub>m</sub> did not
represent a _complete implementation_ of _measurement units_ -- as
_measurement units_ being of definitions standardized in _Systeme
International_ and as addressed in other institutional
publications.

Ig<sup>1</sup><sub>m</sub> was designed, essentially, as an initial
prototype for a mathematical system. That prototype now 
being further developed, in Ig<sup>2</sup><sub>m</sub>, the discussion
will return to continue discussing the issues of the ambiguity of
_magnitude_ and _numeric value_, in Ig<sup>1</sup><sub>m</sub>.


#### Similarity of Concepts onto Implementations of Floating-Point Arithmetic

As regarding how a_numeric value_ may be encoded of seperate
_magnitude_,  _exponent base_ and _exponent degree_ values, there may
seem to be some generic similarities towards implementations of
_floating point_ numeric systems. Excepting that the  _magnitude_ and
the _degree_ would both be iplemented as _rational values_ -- as
within Ig<sup>1</sup><sub>m</sub>  -- respectively, those concepts
are analogous toe the concepts of _mantissa_ and _exponent_, as
represnted in IEEE standard _floating point_ arithmetic.

Presently, that analogy will not be further developed of this article
of documentaiton. If it may serve towards a development of a numeric
system for a microprocessor, but considering the braoder industry
adoption of IEEE _floating point_ arithmetic, any further thesis as
such should well be accompanied with a substantial _proof of
concept_.

In one further divergence from the IEEE standard model for _floating
point_ arithmetic: Essentially, this system of _numeric values_
represents not a _floating point_ numeric implementation -- though it
may naturally apply a _floating point_ numeric implementation, insfoar
as applications of standard implementations of numeric functions
developed in CLtL2, typically implemented onto LibC.

Rather, the details of the design of this numeric system may be more
characteristic of a _decimal, "fixed point"_ numeric implementation,
but in which each _decimal_ value would be represented with a
_rational_ encoding -- such that the _rational_ encoding of a decimal
numeric value may serve to permit for mathematical operations mostly,
if not entirely on rational numbers -- ideally, as to minimize errors
that may result of _floating point contagion_ and furthermore, of 
_feedback_ within computational implementations of numeric systems. 

#### Conclusion - Numeric Value and Rational Magnitude

In the development of Ig<sup>2</sup><sub>m</sub>, a further
distinction must be developed between concepts of _numeric value_ and
of _rational magnitude_.  This may entail a substantial revision
to the design of Ig<sup>1</sup><sub>m</sub>, insofar as towards
specific functional forms developed originally in
Ig<sup>1</sup><sub>m</sub>.

Insofar as towards an offhand analysis of possible _side effects_ of
such revisions: Ig<sup>1</sup><sub>m</sub> was not widely publicized
for its development. If it may be believed that any additional
developers may have located the source code of
Ig<sup>1</sup><sub>m</sub>, in the source repository for
[igneous-math, at Github][igneous-math] -- such that, hypothetically,
in a functional manner, would be _possible_ to anyone with Internet
access, though it may seem likewise an _unlikely possibility_, in any
offhand estimation -- then perhaps it may also be believed that any 
developer having located the same source code repository, at Github,
may be of an understanding that Ig<sup>1</sup><sub>m</sub> was by no
means anything but a prototype, and should by no means be placed into
any broad adoption. Likewise, it is a prototype to which
Ig<sup>2</sup><sub>m</sub> may not demonstrate any absolute _backwards 
compatibility_.

#### Towards a Reference Implementation for Numeric Calculations in Common Lisp

As the design of Ig<sup>2</sup><sub>m</sub> continues to evolve, this
article in documentation should be furthermore extended, as to
illustrate the conceptual frameworks developed in Igneous-math, and to
describe their intended applications within distinct _usage case_
scenarios.

Candidly, this system was not designed to be a "Cheat Sheet" for
the homework assigments at any single academic institution, and
neither a "Short circuit" for any informal studies of disciplines in 
mathematics and the sciences of mechanical engineering.

It is hoped that this system may serve to assist in development of a
single _reference implementation_ for numeric calculations applying
_scalar values_, in a comprehensive numeric system. Although there may
not be any single, formal project denoting a development of such a
_reference implementation_, however -- insofar as with regards to
numeric systems developed in Common Lisp -- clearly, at least one
_proof of concept_ may already exist, such that could be considered 
towards development of a unified mathematical model for numeric
applications, such as developed in Common Lisp.

Certainly this project, in itself, could only serve towards any such
_reference implementation_, if only insofar as that this project has
developed any complete solutions to any issues as may be presented to
such _reference implementation_. Towards discovery of such issues, and
towards their further documentation, firstly the developer accepts
that the development of this system represent a substantial divergence
from applications of _existing tools_ in numerical computation. As
Igenous-Math being essentially a software project, this system may not
diverge from _existing methods_ in development and application of
mathematical methods in computational systems.

#### Literary "Outro"

Though it might be remarked of as something of a "Horse of a different
color," in regards to _existing tools_, and though the conceptual
model presented of this project may not be obviously in alignment with
any single institutional model -- the developer of this project being,
moreover, not a student of any single thesis program, academically,
though the developer is formally a student of a postsecondary academic
institution, presently -- but insofar as that the development of this
project may present an opportunity for reevaluation of _existing
methods_ in mathematical computation, and - in a manner of extension -
an opportunity for an evaluation of _alternate approaches_ for
development of computational systems applying of decimal values, but
the developer accepts likewise that it may not result in any too
glaringly brilliant thesis topics.
 
If the reader may be hunting for a brilliant thesis topic, presently, 
the developer would kindly advise that a visit to a history museum may
afford anyone a great opportunity to take a break from hunting.  If
the history museum be overcrowded, then, but so much is there to say
of the natural history of society and the arts, as well as the
artifacts of a museum's curation.

Thus, a discrete character of logic.


## Measurements Implementation in Ig<sup>1</sup><sub>m</sub>

In a synopsis of the semantics of the design of the 
Ig<sup>1</sup><sub>m</sub> _measurements_ module: A _measurement
domain_, in viewed in terms of SI, represents a type of _quantity_ 
(e.g. _temperature_ or _inductance_), whereas a
_measurement class_ represents an SI _measurement_ type (e.g. _degrees 
kelvin_ or _henries_) for quantities of a specific type.

In Ig<sup>1</sup><sub>m</sub>, a _measurement_ is essentially a scalar
object that is an instance of a _measurement class_. Corrspondingly, a
_measurement class_ is an instance of a _measurment domain_. In
Ig<sup>1</sup><sub>m</sub>, _measurement domains_ are implemented as
_metaclasses_.

Correspnding with the _semantic model_ for _measurement domains_, 
_measurement classes_, and _measurement_ objects,
Ig<sup>1</sup><sub>m</sub> implements some qualities of a
presentational model for measurements. See also:
`pretty-printable-object` [[mci-cltl-utils][mci-cltl-utils]]

### Concept: Extensions and Revisions onto Ig<sup>1</sup><sub>m</sub> Measurement Classes

* Class: `MEASUREMENT-CLASS`
    * Protocol class
    * See also: `REGISTER-MEASUREMENT-CLASS`
    * See also: `MEASUREMENT-DOMAIN`
    * See also: `LENGTH`; `MASS`; `TIME`; `ELECTRICAL-CURRENT`;
      `TEMPERATURE`; `AMOUNT-SUBSTANCE`; `LUMINOUS-INTENSITY`;
* Class: `BASE-MEASUREMENT-CLASS`
    * `MEASUREMENT-CLASS` for SI base units
    * See also:  `BASE-LENGTH`; `BASE-MASS`; `BASE-TIME`;
      `BASE-ELECTRICAL-CURRENT`; `BASE-TEMPERATURE`;
      `BASE-AMOUNT-SUBSTANCE`; `BASE-LUMINOUS-INTENSITY`; 
* Class: `DERIVED-MEASUREMENT-CLASS`
    * `BASE-MEASUREMENT-CLASS` and `DERIVED-MEASUREMENT-CLASS` form an
      _exhaustive set_ of types of `MEASUREMENT-CLASS`
    * In application as a metaclass: Essentially, applied for
      definition of measurement units for which there is a single
      linear converstion ratio, for conversions to/from a base
      measurement unit or to/from another derived measurement 
      unit
    * Analous to existing `MEASUREMENT-CLASS` for (linear) derived
      units
    * See also: `CONVERSION-FACTOR`; `FIND-CONVERSION-FACTOR`;
      `MEASUREMENT-DOMAIN`
* Class: `LINEAR-MEASUREMENT-CLASS`
    * Direct superclasses: `DERIVED-MEASUREMENT-CLASS`
    * Semantically correlated with `LINEAR-UNIT-EXPR`
* Class: `GEOMETRIC-MEASUREMENT-CLASS`
    * Direct superclasses: `DERIVED-MEASUREMENT-CLASS`
    * Semantically correlated with `GEOMETRIC-UNIT-EXPR`
* Class: `COMPOUND-MEASUREMENT-CLASS`
    * Direct superclasses: `DERIVED-MEASUREMENT-CLASS`
    * Semantically correlated with `COMPOUND-UNIT-EXPR`



### Concept: Unit Expressions in Ig<sup>2</sup><sub>m</sub>

* Class: `UNIT-EXPR`
    * A _unit expresion_ represents an expression of a _measurement
      unit_.
    * Essentially, this concept pertains to presentation of
      _measurement units_. See also: McCLIM (MCi fork)
    * An implementation of the concept _unit expression_ may be
      implemented as orthogonal to the Ig<sup>1</sup><sub>m</sub>
      `MEASUREMENT-CLASS` -- each `MEASUREMENT-CLASS`, orthogonally,
      being an instance of `MEASUREMENT-DOMAIN` (which itself is a
      class).
    * In Ig<sup>1</sup><sub>m</sub>, seven primary `MEASUREMENT-CLASS`
      classes are defined, each corresponding to a single
      `MEASUREMENT-DOMAIN`.
    * The class `UNIT-EXPR` may be implemented towards a
      `UNIT-EXPR` representing a syntactic expression of a
      _measurement class_
    * Three essential types of _unit expression_
        * **Linear unit expression**, e.g. as denoted with `m` or `W`
            * The corresponding _measurement class_ for each _linear
              unit expression_ may be _indexed_ according to a symbol 
              representing the name assigned to the _measurement
              class_ (e.g `:|m|`) and correspondingly, to the _unit
              expression_
        * **Geometric unit expressions**, e.g. as denoted with `m^2`
            * The corresponding _measurement class_ for each
              _geometric unit expression_ may be _indexed_ according to
              the _linear unit expression_ represented of the
              _geometric unit expression_ and correspondingly, the
              _degree_ of the _geometric unit expression_
            * Semantically, a _unit expression_ `m^2` represents a
              measure of _surface area_, whereas a _unit expression_
              `m^3` represents a measure of _space_. As in  this
              trivial example: Although the respective concepts of
              _surface area_ and _space_,  may not be _numerically
              relevant_ -- such as for formulas either applying or
              producing _measurement values_ of those respective
              _measurement units_ -- but infoar as to represent a
              semantic significance of the respective _measurement
              units_, it may be appropriate to define a _measurement
              domain_ corresponding to each -- respectively, a domain
              for measures of _surface area_ and a domain for measures
              of _cubic space_ or _volume_.
            * **"Special Note**": [[BIPM][#BIPM]] defines the
              _measurement unit_ for the domain, _frequency_ in terms
              of _SI base units_ as `s^-1`(e.g table 3, Eng. p. 118,
              PDF p. 26) 
        * **Compound unit expressions** e.g. as denoted with `m^2 kg s^-3`
            * The corresponding _measurement class_ for each _compound
              unit expression_ may be _indexed_ as a _sequence_ of
              elements [`simple-vector`] `#(U D)` where `U` represents
              a _measurement unit_ and `D` represents a _degree_ of
              that _measurement unit_. For example: `#2A((m 2) (kg 1)
              (s -3))` as a _normalized compound unit expression_.
              corresonding to `W` i.e. `watts`
            * Alternatley, each element of the _measurement elements
              sequence_ for a _compound unit expression_ may be stored
              as a _geometric unit expression_.
            * Orthogonally, towards a presentation model for _compound
              unit expressions_ -- pending a design for _superscript_
              and _subscript_ typesetting in McCLIM -- the
              presentation may be focused mostly about representing
              the syntactic qualities of a unit expressions, homologous
              with unit expressions in igneous-math. Where possible,
              measurement units should be presented in their _derived_
              forms (contrasted to _normalized form_)
            * **"Special Note**": [[BIPM][#BIPM]] defines the
              _measurement unit_ for the domains, _plane angle_ and
              _solid angle_, when in terms of _SI base units_ as
              respectively, `m m^-1` and `m^2 m^-2`. These two
              derivations may represent the only instances in which
              the _base unit expression_ of an _SI measurement unit_
              is defined in a form not _reduced_ for its
              exponents. Orthogonally, those expressions each provide
              a _base measurement unit_ derivation  for, respectively,
              _radian_ and _steradian_
            * **Concept: Normalized compound unit expression**
                * For a _unit expression_ `A_1`, the _normalized
                  compound unit expression_ of `A_1` shall be a 
                  _compound unit expression_ representing the
                  _measurement unit elements_ of `A_1` all converted
                  and reduced to SI _base units_
            * **Concept: Partially normalized compound unit
              expression**
                  * Example: The derived unit, _volt_ (`V`) may be
                    represented of a _compound unit expression_ `W A^-1`
                  * Internal to the _measurements_ module of
                    [[igneous-math][igneous-math]], a _prtially
                    normalized compount unit expression_ may be
                    converted to a fully _normalized unit expression_
                    simply by _normalizing_ each _compound unit_ in
                    the unit expression to any _compound expression_
                    of SI  _base units_
            * For purpose of _normal ordering_ within index search and
              retrieval algorithms, the _order of elements_ in a
              _normalized compound unit expression_ should not be
              arbitrary. This system will specify that the _order of
              elements_ of a _normalized compound unit expression_
              will be in accord with the sequence of _SI base unit_
              names denoted in section 2.1.2 of [[BIPM][#BIPM]]
              (in English language, p. 116, PDF p. 24) See also:
              `+SI-BASE-MEASUREMENTS+` 
* Class: `LINEAR-UNIT-EXPR`
    * Concept: A syntactic container for a single measurement unit
      expression, e.g `m`
    * Concern: The definition of this class would serve to ensure that
      the subclasses specified of `UNIT-EXPR` would   represent
      an _exhaustive set_ of types of _unit expression_. However,
      this class -- in its application -- might seem redundant onto 
      `SYMBOL`, insofar as that a _symbol_ may be applied as being
      syntactically representative of a _measurement unit_. Insofar as
      that -- conversely -- a _symbol_ would be interpreted as being
      representative of a _measurement unit_, the classes `SYMBOL` and
      `LINEAR-UNIT-EXPR` are therefore _functionally
      orthogonal_. Effectively, a `LINEAR-UNIT-EXPR` would
      represent an encapsulation of a `MEASUREMENT-CLASS`, insofar as
      input and presentation of _unit expressions_. Transitively, a
      `MEASUREMENT-CLASS` may be denoted with a _symbol_, as in the
      context of the _measurement classes namespace_
* Class: `GEOMETRIC-UNIT-EXPR`
    * Concept: A syntactic container for both of a single measuremnt
      unit expression and a numeric (fixnum) degree of the measurement
      unit
* Class: `COMPOUND-UNIT-EXPR`
* Function: `SIMPLIFY-UNIT`
    * Syntax and arguments: `SIMPLIFY-UNIT EXPR => EXPR`
       * `EXPR`: A _unit expression_
    * Description (design): Provided a _partially normalized unit
      expression_, this function must produce a _partially normalized
      unit expression_ in which -- insofar as possible -- any
      combinations of _base units_ representative of a _derived unit_
      would be expressed as the corresponding _derived unit_
    * Implementation Notes
        * Indexing of measurement units
           * Given a _compound unit expression_ -- whether expressed
             in _derived units_ such as `W A^-1`, or expressed in _base
             units_ such as ``m^2 kg s^-3 A^-1`, or in any combination
             of _base units_ and _derived units_, and in any exponent
             prefix of those _unit expression elements_ -- such that
             the _compound unit expression_ would be  representative
             of an _SI derived unit_(e.g. _volt_) each _compound
             unit expression_ must be stored within a central
             _compound unit expressions index_. An interface must be
             defined onto the _compound unit expressions index_, such
             that a _compound unit expression_ can be stored within
             the index and later retrieved
* Function: `NORMALIZE-UNIT`
    * Syntax and arguments: `SIMPLIFY-UNIT EXPR => EXPR`
       * `EXPR`: A _unit expression_
    * Description (design) : Recriprocal to `SIMPLIFY-UNIT`,
      essentially this function must produce a _fully normalized unit
      expression_ for the provided `EXPR`

## TO DO (Ig<sup>1</sup><sub>m</sub>)

* Define measurement domains for derived measurement units
    * "Normalize" and document those measurement domains already
      defined within this software system
    * Refer to [SI], [NIST], other resources 
    * e.g. towards units of amps, watts, ohms, newtons, radians, herz...


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
> => #&lt;METER 1 m {10083AF7A3}&gt;
>
>  (class-of *m*)
>  => #&lt;BASE-LENGTH METER&gt;
>
>  (class-of (class-of *M*))
> => #&lt;MEASUREMENT-DOMAIN BASE-LENGTH&gt;


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


#### Topic: Measurement Formulas

The measurement unit, _ohm_, as a standard unit for measurement of
_electrial resistance_, is defined as a derived unit with formulas 

    1 ohm = 1 m^2 kg s^-3 A^-2	[1] p. 111
    i.e. ((:|m| 2) :|kg| (:|s| -3) (:A -2))
    
    1 ohm = 1 V / 1 A		[1] p. 118

Similarly, the measurement unit, volt, as a standard unit for
measurement of electromotive force or difference in electrical
potential, is defined with formulas

    1 volt = 1 m^2 kg s^-3 A^-1	[1] p. 118
    1 volt = 1 W / 1 A		[1] p. 118

and the measurement unit, watt, as a standard unit for measurement of
power or radiant flux:

    1 watt = 1 m^2 kg s^-3		[1] p. 118
    1 watt = 1 J / 1 S		[1] p. 118

and the measurement unit, joule, as a standard unit for measurement of
work, energy, or amount of heat:

    1 joule = 1 m^2 kg s^-2	[1] p. 118
    1 joule = 1 N * 1 M		[1] p. 118

and the measurement unit, newton, as a standard unit for force:

    1 newton = 1 m kg s^-2		[1] p. 118

Thus, a definition of the derived measurement unit, ohm, effectively
requires the  definitions of the derived measurement units: newton,
joule, watt, and volt.


[1] <http://www.bipm.org/en/publications/si-brochure/>

#### Topic: Measurement Conversion

Issue: Towards a methodology for linear unit conversion

NIST SP 811[1] appendix B defines an effective table of conversion
factors for standard measurement units recognized by the NIST. The
table is defined with a format, effectively: (S,D,F,E) for

    S: source measurement unit
    D: destination measurement unit
    F: factor for converstion
    E: decimal exponent for factor of conversion

S and D do not appear uniquely in the table, but the set {S,D) is
unique within the table.

In transposing that measurement conversions table into this software
system, a number of matters may be considered, for example:

1. That a floating-point value 3.048 E-03 (imay be represented similarly
   as 3048 E-6, with the exponent value stored seperate to the integral
   magnitude -- thus, ensuring that a manner of integral arithmetic
   may be applied onto the integral magnitude and its integral decimal
   degree.

2. that for any measurement converstion path A..D in which the
   following measurement conversion paths are available:
    * A..B
    * B..C
    * C..D
   ...the converstion from A to D may be accomplished directly by
   multiplying the factors for the conversions A..B, B..C, and C..D
   and summing their decimal degrees

   

NIST SP 811[1] clause B.9, furthermore, defines a set of individual
measurement domains and subdomains.


[1] NIST. Guide for the Use of the International System of Units
    <http://physics.nist.gov/cuu/pdf/sp811.pdf>

#### Formal Measurement Definitions in HyTime

Point of reference: <http://www.hytime.org/materials/hi2mdhyt.sgm>

Notation names and public identifiers, for 'standard measurement unit'
(SMU) definitions,from the _HyTime Granule Definition Notation_ module
of "ISO/IEC 10744:1997", i.e. _Hypermedia/Time-based Structuring
Language_ (HyTime)

* `gQuantum "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"`
* `SIsecond "ISO/IEC 10744:1997//NOTATION Systeme International second//EN"`
* `SImeter "ISO/IEC 10744:1997//NOTATION Systeme International meter//EN"`
* `virTime "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"`
* `virSpace "ISO/IEC 10744:1997//NOTATION Virtual Measurement Unit//EN"`
* `SIkg "ISO/IEC 10744:1997//NOTATION Systeme International kilogram//EN"`
* `SIcd ""ISO/IEC 10744:1997//NOTATION Systeme International candela//EN"`
* `SIampere "ISO/IEC 10744:1997//NOTATION Systeme International ampere//EN"`
* `SImole "ISO/IEC 10744:1997//NOTATION Systeme International  mole//EN"`
* `SIradian "ISO/IEC 10744:1997//NOTATION Systeme International radian//EN"`
* `SIsr "ISO/IEC 10744:1997//NOTATION Systeme International steradian//EN"`

Issue: The _gQuantum_, _virTime_ and _virSpace_ notations share the same
public idenifier, but may be differentiated by their respective
notation names. Though practically useful, however those
measurement units are not standardized onto SI

see also:

* <http://crism.maden.org/consulting/pub/hytime/meas.html> (1992)
* <http://www.is-thought.co.uk/schedule.htm>
* <http://www.hytime.org/materials/hi2mdhyt.sgm>
    * cf. %hygrand


**Previous Documentation (2) :**

Referring to [BIPM][#BIPM], p. 118 (English Edition PDF p. 26) a
number of additional _measurement domains_ are defined within a single
table, each denotedper an essential type of _quantity_. Within a
domain of applications of electrical systems, the folloiwng units are
denoted of that table:

* Volt (V)
    * Domain: _Electrical power difference, electromotive force_
	* Relation in terms of SI derived units: `W/A`
	* Relation in terms of SI base units: 
	* Common Applications include _Ohm's Law_ -- in a classic form,
	  `I=V/R`
	* ....

**Previous Documentation (1) :**

The measurement unit, _ohm_, as a standard unit for measurement of
quantity of _electrical resistance_, is defined as a derived unit with
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


`[1]` <http://www.bipm.org/en/publications/si-brochure/>


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
> => #&lt;METER 1 m {10083AF7A3}&gt;
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

     _FIXME: This may be effectively outdated together with the
     definition of reader macros for measurement unit expressions_ 

   * For measurement units defined by SI[BIPM][#BIPM] with subscript characters,
     a corresponding symbolic name shall be sought of the NIST
     specification[NIST][#NIST] 

     e.g symbolic name `"Lᵥ"` in SI[BIPM][#BIPM], interpreted rather as `"lx"` in
     NIST[NIST][#NIST] and therefore `=> :lx`


   * For measurement units whose conventional symbolic name denotes a
     ratio among measurement units, the character denoting the ratio
     shall be retained

     e.g `"kg/m³" => :kg/m^3`

    _FIXME: This is effectively outdated with the addition of compound
    measurement unit expressions_ 
   
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
> => #&lt;METER 1 m {10083AF7A3}&gt;
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

### Additional Resources

* [IUPAC 'Gold Book'](http://goldbook.iupac.org/list_math.html)
  esp. <http://goldbook.iupac.org/list_goldbook_quantities_defs_A.html>
* [NASA SWEET Ontologies](http://sweet.jpl.nasa.gov/)
* <http://physics.nist.gov/cuu/units/>
* <http://physics.nist.gov/pubs/sp811/appenb.html>
    * ^ esp. for conversions regarding foot, mile, yard , ...

#### Appendix. Notes Towards Integration with Measurement Systems

* Sidebar; Measurement tools (e.g. oscilloscope; digital multimeter;
  thermometer; barometer; soil moisture sensor; solar intensity  ...)
* Sidebar: Experimental data (e.g. spectrographs; astrometry data)
* A primary concern: "Rigorous metadata recording," i.e insofar as to record the source of a calculation or a measurement, as "measurement metadata," for purpose of reference - cf. [LoC MODS RDF](http://www.loc.gov/standards/mods/modsrdf/), [LoC PREMIS](http://www.loc.gov/standards/premis/)
    * If a numeric value is a result of a calculation, it should be annotated as a calculation, and annotated for the methodology in which the calculation was derived
    * If a numeric value is a result of a measurement, it should be annoted with the identity of the device by which the measurement was obtained, as well as the configuration of the measurement system in which the device was applied.
    * In effect, this requires a development of an object model for measurement devices. See also: [EDIF], [ISO-10303], [STEP]; JEDEC; [About STEP](http://www.ida-step.net/support/resources/about-step), but not as if to neglect the derivation of parts of the STEP standard from EDIF;
    * For interactive/manual calculations and measurements, an applicaiton may request user input for establishing the respective source of calculation or measurement
    * For automated calculations or measurements with multiple heterogenous data sources, a data model may be developed such that would effectively "tag" each measured value with the identity of the measurement system from which the value was "read"

##### Measurement Workflows

(TO DO - look for NASA best practice guidelines, regarding? i.e. "Lab work")


#### Display Procedures

#### Oscilloscope Data

* Graph
    * Bezier curves ?
    * Sample data ?


#### ...



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
