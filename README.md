
## measurement unit structure

the systeme internationale (si) defines seven base measurement units,
of which any addditional measurement units may be directly ro
indirectly derived[1]. this program system represents those respective
measurement units as corresponding to measurement domains.

* length
* mass
* time, duratin
* electric current
* thermodynamic temperature
* amount of substance
* luminous intensity

each of the fundamental measurement domains is defined with a
corresponding base unit.

* length: meter (alternately, metre)
* mass:  kilogram
* time, duration: second
* electric current: ampere
* thermodynamic temperature: kelvin
* amount of substance: mole
* luminous intesity: candela

in standards published by the si, and in standards published by the
nist, those base units are subsequently extended with formulas
describing the mathematical natures of conventional derived
units. [1][2]

### conventional prefixes for decimal multiples of measurements

the systeme internationale defines a standard set of symbolic names
for decimal exponents of measurement values, in a range from
-24 to 24 degrees. conventionally referred to as "engineering
notation"[3], these decimal prefixes allow for a succinct
representation of the magnitudes of measurement values.

### measurement accuracy and significant digits

(to do)

cf. http://www.degruyter.com/view/j/cclm.2004.42.issue-9/cclm.2004.216/cclm.2004.216.xml

### structure of virtual measurement units

in addition to the si base units, this system endeavors to define a
number of additional measurement domain, inspired by the hytime
standard[4] specifically an abstract "virtual measurement"
domain. the "virtual measurement" domain is extended with non-abstract
base units, "virtual time" and "virutal space", as for purpose of
representing measurable quanities within a software system, such as
pal frame rate and pixel coordinates[5]

## measurement unit names

this program system will endeavor to adopt a consistent syntax for
measurement names, within the context of this program system.

consulting the si brochure[1], all of the seven si base units[1] are
defined, each, around a specific measurement domain, denoted by a type
of base quantity.

each type of base quantity is assigned with one or more conventional
symbols, such as for variables denoting of such a quantity. the
applications may vary, for those conventional symbols for quantities
of the base unit -- for example, "l" being applied commonly for
measure of linear dimesion, and "r" applied commonly for radial
magnitude, each of the previous  denoted as a conventional symbol for
a variable denoting of a measure of length. this application system
will not make a formal defintion of those conventional symbols for
variables denoting of quantity. 

each type of measurement base quantity corresponds to exactly one
measurement base unit. 

each base unit is denoted with a unit name -- for instance, "second"
-- and one or more symbols denoting of the measurement unit -- for
instance "s". 

for purpose of referene within software applications, this program
system will endeavor to define a normative syntax for measurement
units, consistent with common practice. in that context, it is a 
requirement of this system that all measurement units defined by this
system may be referenced with a symbolic name comprised of printing
characters within the ascii character set.

in common practice, some measurement units are denoted with non-ascii
characters and qualities of typesetting -- including subscript
characters and greek letter characters -- some of which are available
however, within the unicode code set. 

this program system will endeavor to define the following values for
each type of measurment unit (slots of class, measurement-unit)

* quantity-name - name of base quantity, e.g "time, duration"

* print-label - a singular, verbose, printable name for the measurement
  unit, wtihout symbolic characters e.g. "meter", "newton meter"

* print-name - a singular, printable name for symbolic
  representation of the measurment unit in conventional syntax -- in 
  which instance, this system assumes that the lisp implementation
  implements the unicode code set -- e.g. "m", "°", "ω", or "lᵥ"

* symbol - A Lisp symbolic name, interned within the keyword package, for
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
     this system shall use the letter forms respectively :|'| and
     :|"|, with printed representation respectively "'" and '"'

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


## Geometric Object Model

This system is being developed for a purpose of defining a consistent
measurement model for application in analytic geometry. In order to
ensure that this system would provide a model consistent onto
standards for conventional practice in measurement modeling, and would
implement a mathematically reliable methdology for conversion of
metric values, this system's development effectively must be informed
of standards for "best practice."

In extending of the measurement model ipmlemented of this proram
system,  a further set of components may be defined for representation
of properties of trigonmetic systems as well as known practices in
analytic geometry. within the Common Lisp programming environment. 

For representation of measurement values, this system defines a single
class, SCALAR, namely for representation of quantities of known
measurement units. The class, SCALAR, is then extended with the class,
VECTOR, in a programmatic model ipmlemented, internally, onto the
polar coordinate plane. 

For purpose of ensuring compatibility with mathematical methods
utilizing of qualities of the Cartesian/Euclidian and rectangular
coordinate planes, this system furthermore defines an object model for
representation of geometric forms, specifically within a Common Lisp
programming environment.


[1] BIPM. SI Brochure: The International System of Units (SI) [8th
    edition, 2006; updated in 2014]
    available: http://www.bipm.org/en/publications/si-brochure/ 

[2] NIST. Guide for the Use of the International System of Units (SI)
    available: http://www.nist.gov/pml/pubs/sp811/index.cfm 

[3] The Engineering Toolbox. Stanard Form, Scientific, and Engineering
    Notation.
    available: http://www.engineeringtoolbox.com/standard-form-scientific-engineering-notation-d_1801.html 

[4] HyTime Granule Definition Notation. 
    available: http://www.hytime.org/materials/hi2mdhyt.sgm

[5] Martin Bryan. Using HyTime for Scheduling Events
    available: http://www.is-thought.co.uk/schedule.htm


## TO DO

1) Define an ASDF system for this source file

X) Define a reader macro syntax for measurement units
   also cf. Jakub Higersberger's unit-formula system

Y) Extend MEASUREMENT-CLASS with a new class,
   DERIVED-MEASUREMENT-CLASS, such that would publish an accessor
   for calculating a formula for measurement unit converstions

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

9) Define a desktop interface for this system - carefully avoiding
   any manner of a competitive spirit towards Wolfram Mathematica
   and the respective MathML implementations available within the
   contemporary computing domain - albeit all within proprietary/
   closed source software systems, "those" -- also maxing reference
   onto the ACL2 "theorem prover" system  and Maxima. Although this
   system is being defined morso for a purpose of supporting
   applicationso of analytic geometry, as primarily with regards
   to electical engineering, however there must be some references
   made onto theoretical mathematics, throughout this system.


FIXME: The following documentation items were transposed from
measurement.lisp and should be edited for presentation in this
_markdown_ file

## Sidebar: Measurement Definitions in HyTime

* http://www.is-thought.co.uk/schedule.htm
* http://crism.maden.org/consulting/pub/hytime/meas.html
* http://www.hytime.org/materials/hi2mdhyt.sgm
    * ^ cf %hygrand

see also:
* http://physics.nist.gov/cuu/units/
* http://physics.nist.gov/pubs/sp811/appenb.html 
    * ^ esp. for conversions regarding foot, mile, yard , ...


