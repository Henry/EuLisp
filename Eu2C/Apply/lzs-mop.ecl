;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Title: A part of the EuLisp Meta Object Protocol used at Compile Time
;;;  Description:
;;    This module provides some subclasses of the LZS-class <class-def> for a
;;    better division of standard-, meta- and tail-classes.  This module also
;;    provides functions of the EuLisp-MOP which are needed by the compiler to
;;    compute resp. initialize things like classes and generic functions at
;;    compile time. The domain of these functions are the compile time
;;    representations (LZS representation) of classes, generic functions etc.
;;    The naming is done by prefixing the original names of the EuLisp MOP by a
;;    tilde.  For example ~class-precedence-list is a function like
;;    class-precedence-list of EuLisp but works with LZS representations of
;;    classes.
;;;  Notes:
;;    1. The arguments of the ~-functions are not exactly the same as for their
;;    EuLisp counterparts. They differ in structure. For example
;;    ~compute-slots gets a list of slot descriptions instead a list
;;    of slot specifications (the list form).
;;
;;    2. The following activation order must be taken into account during class
;;    initailization (read top down, left right):
;;    ~compute-representation
;;    |            |
;;    ~compute-runtime-initialization   ~compute-reader/writer
;;    |
;;    ~compute-allocator
;;    |
;;    ~compute-constructor
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module lzs-mop
(import (level-1
         accessors
         lzs
         el2lzs
         el2lzs-error
         (only (*basic-system*)
               predicates)
         (only (remove-duplicates
                substitute
                find
                append
                mapc
                make-instance)
               common-lisp))
 syntax (level-1
         apply-standard
         el2lzs)
 ;;--- lzs specializations
 expose ((only (<standard-class-def>
                <abstract-class-def>
                <tail-class-def>
                <metaclass-def>
                <slot-accessor-fun>
                <slot-init-fun>
                <constructor-fun>)
               lzs))

 ;;--- predefined classes for applications
 export ($<object>
         $<class>
         $<tail-class>
         $<abstract-class>

         create-predefined-standard-classes)

 ;;--- General
 export (~initialize
         ~class-of
         ~subclass?)

 export (%object
         %class
         %abstract-class
         %tail-class)

 ;;--- Class Introspection
 export (~class-name
         ~class-precedence-list
         ~class-slots
         ~class-keywords
         ~class-representation
         ~find-slot
         ~class-subclasses
         ~converter)

 ;;--- Slot Introspection
 export (~slot-name
         ~slot-default-function
         ~slot-slot-reader
         ~slot-slot-writer
         ~slot-type
         ~slot-keyword
         ~slot-initvalue)

 ;;--- Generic Function Introspection
 export (~generic-function-domain
         ~generic-function-method-class
         ~generic-function-methods
         ~generic-function-method-lookup-function
         ~generic-function-discriminating-function
         ~generic-function-discrimination-depth)

 ;;--- Method Introspection
 export (~method-domain
         ~method-function
         ~method-generic-function)

 ;;--- Class Initialization
 export (~compute-class-precedence-list
         ~compute-inherited-slots
         ~compute-slots
         ~compute-inherited-keywords
         ~compute-keywords
         ~compute-representation
         ~compute-lattice-type
         ~compute-constructor
         ~compute-predicate
         ~compute-runtime-initialization
         ~compute-allocator)

 ;;--- Inheritance Protocol
 export (~compute-and-ensure-slot-accessors
         ~compute-slot-reader
         ~compute-slot-writer)

 ;;--- Method Lookup and Generic Dispatch
 export (~compute-method-lookup-function
         ~compute-discriminating-function
         ~add-method
         ~compute-discrimination-depth
         ~compute-discrimination-arguments)

 ;;--- Introspection of Vector Classes
 export (~vector-class-instance-size
         ~vector-class-instance-size-literal
         ~vector-class-element-type
         ~vector-class-element-default-function)

 ) ;end of module header

;;;-----------------------------------------------------------------------------
;;; predefined classes for applications
;;;-----------------------------------------------------------------------------
;;; The following classes are placed as top-lexical bindings into the basic
;;; application module TAIL.

;;(define-tail %object export (abstract-class-def)
;;  :supers ()
;;  :direct-slots ()
;;  :representation ()
;;  )

(deflocal %object ())

;;(define-tail %class export (metaclass-def)
;;  :supers (list %object)
;;  :representation ()
;;  )

(deflocal %class ())

;;(define-tail %abstract-class export (metaclass-def)
;;  :supers (list %class)
;;  :class %class
;;  :representation ()
;;  )

(deflocal %abstract-class ())

;;(define-tail %tail-class export (metaclass-def)
;;  :supers (list %class)
;;  :class %class
;;  :representation ()
;;  )

(deflocal %tail-class ())

(defun create-predefined-standard-classes ()
  (setq %object
        (make-instance (if (null? *basic-system*)
                           <abstract-class-def> <imported-class>)
                       :identifier ^%object
                       :code-identifier "c__object__level_0"
                       :module $tail-module
                       :supers ()
                       :direct-slots ()
                       :representation ()))
  (add-lexical %object $tail-module ^export)

  (setq %class
        (make-instance (if (null? *basic-system*)
                           <metaclass-def> <imported-class>)
                       :identifier ^%class
                       :code-identifier "c__class__level_0"
                       :module $tail-module
                       :supers (list %object)
                       :representation ()))
  (add-lexical %class $tail-module ^export)

  (setq %abstract-class
        (make-instance (if (null? *basic-system*)
                           <metaclass-def> <imported-class>)
                       :identifier ^%abstract-class
                       :code-identifier "c__abstract_class__level_0"
                       :module $tail-module
                       :supers (list %class)
                       :class %class
                       :representation ()))
  (add-lexical %abstract-class $tail-module ^export)

  (setq %tail-class
        (make-instance (if (null? *basic-system*)
                           <metaclass-def> <imported-class>)
                       :identifier ^%tail-class
                       :code-identifier "c__tail_class__level_0"
                       :module $tail-module
                       :supers (list %class)
                       :class %class
                       :representation ()))
  (add-lexical %tail-class $tail-module ^export)


  (setf (?class %object) %abstract-class)
  (setf (?class %class) %class))

;;;-----------------------------------------------------------------------------
;;; General
;;;-----------------------------------------------------------------------------
(defgeneric ~initialize (object initlist))

(defgeneric ~class-of (object))

(defun ~subclass? (sub super)
  (member super (?class-precedence-list sub)))

;;;-----------------------------------------------------------------------------
;;; Class Introspection
;;;-----------------------------------------------------------------------------
(defgeneric ~class-name (class))

(defgeneric ~class-precedence-list (class))

(defgeneric ~class-slots (class))

(defgeneric ~class-keywords (class))

(defgeneric ~class-representation (class))

(defgeneric ~find-slot (class slot-name))

(defgeneric ~class-subclasses (class))

(defgeneric ~converter (class))

;;;-----------------------------------------------------------------------------
;;; Slot Introspection
;;;-----------------------------------------------------------------------------
(defgeneric ~slot-name (slot))

(defgeneric ~slot-default-function (slot))

(defgeneric ~slot-slot-reader (slot))

(defgeneric ~slot-slot-writer (slot))

(defgeneric ~slot-type (slot)) ;returns the class for slot values

(defgeneric ~slot-keyword (slot))

(defgeneric ~slot-initvalue (slot))

;;;-----------------------------------------------------------------------------
;;; Generic Function Introspection
;;;-----------------------------------------------------------------------------
(defgeneric ~generic-function-domain (generic-fun))

(defgeneric ~generic-function-method-class (generic-fun))

(defgeneric ~generic-function-methods (generic-fun))

(defgeneric ~generic-function-method-lookup-function (generic-fun))

(defgeneric ~generic-function-discriminating-function (generic-fun))

(defgeneric ~generic-function-discrimination-depth (generic-fun))

;;;-----------------------------------------------------------------------------
;;; Method Introspection
;;;-----------------------------------------------------------------------------
(defgeneric ~method-domain (method-def))

(defgeneric ~method-function (method-def))

(defgeneric ~method-generic-function (method-def))

;;;-----------------------------------------------------------------------------
;;; Class Initialization
;;;-----------------------------------------------------------------------------
(defgeneric ~compute-class-precedence-list (class direct-superclasses))

(defgeneric ~compute-inherited-slots (class direct-superclasses))

(defgeneric ~compute-slots (class direct-slot-specs
                                              inherited-slots))

(defgeneric ~compute-inherited-keywords (class direct-superclasses))

(defgeneric ~compute-keywords (class keywords inherited-keyword-lists))

(defgeneric ~compute-representation (class representation-spec
                                           allocation-spec))

(defgeneric ~compute-lattice-type (class direct-superclasses
                                         direct-super-lattice-types))

(defgeneric ~compute-constructor (class parameters))

(defgeneric ~compute-predicate (class))

(defgeneric ~compute-runtime-initialization (class))
(defgeneric ~compute-allocator (class))

;;;-----------------------------------------------------------------------------
;;; Inheritance Protocol
;;;-----------------------------------------------------------------------------
(defgeneric ~compute-and-ensure-slot-accessors
  (class effective-slots inherited-slots))

(defgeneric ~compute-slot-reader
  (class slot effective-slots))

(defgeneric ~compute-slot-writer
  (class slot effective-slots))

;;;-----------------------------------------------------------------------------
;;; Method Lookup and Generic Dispatch
;;;-----------------------------------------------------------------------------
(defgeneric ~set-discriminating-function (generic-fun)
  ;;called after loading application modules and before analyzing functions
  ;;this means that all statically collected methods are stored in the
  ;;generic-fun, but a call of add-method at runtime may add additional methods
  )

(defgeneric ~compute-method-lookup-function (generic-fun domain))

(defgeneric ~compute-discriminating-function (generic-fun domain
                                                          lookup-fn methods))

(defgeneric ~add-method (generic-fun method))

(defgeneric ~compute-discrimination-depth (generic-fun))

(defgeneric ~compute-discrimination-arguments (generic-fun))

;;;-----------------------------------------------------------------------------
;;; Introspection of Vector Classes
;;;-----------------------------------------------------------------------------
(defgeneric ~vector-class-instance-size (vector-class))

(defgeneric ~vector-class-instance-size-literal (vector-class))

(defgeneric ~vector-class-element-type (vector-class))

(defgeneric ~vector-class-element-default-function (vector-class))

;;;-----------------------------------------------------------------------------
;;; Errors
;;;-----------------------------------------------------------------------------
(defun error-redefinition-of-inherited-keyword (slot inherited-slot keyword)
  (frontend-message ^error
                    "Attempt to add ~A as new keyword~
                  for slot ~A of class ~A.~
                  The inherited keyword is ~A from class ~A."
                    keyword
                    (~slot-name slot)
                    (~class-name (?slot-of slot))
                    (?keyword inherited-slot)
                    (~class-name (?slot-of inherited-slot))
                    ))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
