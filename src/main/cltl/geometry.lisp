
(in-package #:math)

;;; % Geometry Domain

;; TO DO

(defclass scalar (measurement) 
  ;; effectively a coordinate onto a number line
  ())


;;; % Concepts of Plane and Space


;; (defclass plane ...)

;; (defclass euclidian-plane ...)
;; ^ alternate name for cartesian coordinate plane ?(?)
;;
;; (defclass rectangular-plane ...)
;; ^ a coordiate "two space" onto (i, j), complex number system

;; (defclass space ...)
;; ... reference vectors ...

;;; % Concepts of Point and Location

;; The term "Coordinate" will be applied informaly, in the following
;;
;; (defclass diadic-coordinate ...) 
;; ^ a <point> onto a <planar surface>
;;
;; (defclass triadic-coordinate ...) 
;; ^ a <point> onto orthogonal <3 space>
;;
;; (defclass polar-coordinate ...) 
;; ^ a <point> within a <polar coordinate> system  on a <planar surface>
;;
;; (defclass spherical-coordinate ...)
;; ^ a <point> within a <spherical coordinate space>,
;;   using a single reference model for spherical coordinates
;;   cf. RA, AZ, DEC, and broader astrometry

;;; % VECTOR as a formal mathematical object type

;; Note that this sytem will use radian notation for vectors, internally

;; (def-frob* *null-vector* ...)

;; (defun null-vector-p vector ...)

;; (def-frob @= vector vector)
;; (def-frob @+ vector vector)
;; (def-frob @* scalar vector) ;; vector dot product
;; (def-frob @* vector vector) ;; vector cross product

;; (def-frob @=@ &rest objects) ;; tail recursion

;; thence to extend this system onto the integral and differential
;; calculus, with applications for frequency domain analysis of 
;; electronic system components

;; to do: add a seperate CLIM presentations system, focusing on
;; applications within electronic design and analysis,
;; and emphasiziing CLIM's definition of a standard approach for
;; visualization of arbitrary coordinate systems onto a planar
;; display screen
