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
;;; Title: 
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: 
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module representation

(import
 (level-1
  lzs
  (only (<tail-class-def>) lzs-mop)
  ;;accessors
  )

 syntax
 (level-1
  (only (defstandardclass) apply-standard)
  )

 export
 (  <basic-class-def>
    <%integer>
    <%signed>
    <%unsigned>
    <%float> ;new name
    <%aux-type>
    <%representation>
    <%machine-type>
    <%pointer> ;superclass of all pointer representations
    <%pointer-to-struct>
    <%pointer-to-vector>
    <%pointer-to-void>
    <%direct>
    <address-expr>
    ?alignment
    ?allocation
    ?byte-size
    ?byte-size-of-instance
    ?byte-size-as-component
    ?mm-type
    ?mm-card

    )
 )


(defgeneric ?alignment (obj))
(defgeneric ?allocation (obj))
(defgeneric ?byte-size (obj))
(defgeneric ?byte-size-as-component (pobj))
(defgeneric ?byte-size-of-instance (obj))
(defgeneric ?mm-type (obj))
(defgeneric ?mm-card(obj))




(defstandardclass <%representation> ()
  (alignment :accessor :initarg)    ;alignment (of all types)

  (byte-size :accessor :initarg)  ;length (in bytes) of all types

  (allocation :accessor :initarg :initform ())  ;slots
  (mm-type  :accessor :initarg :initform ())      ;for the
  (mm-card :accessor :initarg)      ;memory management
  )

;;make a new class for %machine-type
;;%bit ...%quad-word are instances from this class and are holded
;;on the corresponding constants
(defstandardclass <%machine-type> (<%representation>) ())


(defstandardclass <%aux-type> (<%representation>) ())

;;;the basic-data-type = basic-class-def takes informations
;;;from %types = tail-class-def
;;;and from %representation
;;;!!!the old one!!!



;;;but I think it is better to cut the representation from the
;;;eulisp hierarchy and take
;;;!!!the new one!!!

(defstandardclass <basic-class-def> (<imported-class>))

(defstandardclass <%integer> (<basic-class-def>) ())

(defstandardclass <%float> (<basic-class-def>) ())


;;; take <%signed> and <%unsigned> as mixin and not as subclass
;;; from <%integer-def>

;;; mixins for <%signed> and <%unsigned>
(defstandardclass <%signed> () ())

(defstandardclass <%unsigned> () ())

(defstandardclass <%pointer> (<%representation>)
  (offset :accessor :initarg)
  )

(defstandardclass <%pointer-to-struct> (<%pointer>))

(defstandardclass <%pointer-to-vector> (<%pointer>))

(defstandardclass <%pointer-to-void> (<%pointer>)
  (alignment :initform 4)
  (byte-size :initform 4)
  )

(defstandardclass <%direct> (<%representation>))


#module-end
