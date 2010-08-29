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
;;;  Title: basic definitions to realize number classes
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule basic-number
  (import (apply-level-1
           ti-sys-signatures)
   syntax (%tail)
   export (<number>
           <integer>
           <fixed-precision-integer>
           make-fpint
           make-swi))

(%define-abstract-class (<number> <abstract-class>)
  <object>
  ())

(%define-abstract-class (<integer> <abstract-class>)
  <number>
  ())

(%define-standard-class (<fixed-precision-integer> <class>)
  <integer>
  ((fpint type %signed-word-integer
          reader fpint))
  ;;constructor (make-fpint fpint) will be handmaded
  representation direct)


#+(:fixed-precision-integer :big)
(%define-function (make-fpint <fixed-precision-integer>)
  ((val %signed-word-integer))
  (%cast <fixed-precision-integer>
         (%or (%plus val val) #%i1)))

#+(:fixed-precision-integer :big)
(%define-function (make-swi %signed-word-integer)
  ((val <fixed-precision-integer>))
  (%ashiftr (fpint val) #%I1))


#-(:fixed-precision-integer :big)
(%define-function (make-fpint <fixed-precision-integer>)
  ((val %signed-word-integer))
  (%cast <fixed-precision-integer>
         (%ashiftr (%lshiftl val #%I16) #%I16)))

#-(:fixed-precision-integer :big)
(%define-function (make-swi %signed-word-integer)
  ((val <fixed-precision-integer>))
  (fpint val))


;;;-----------------------------------------------------------------------------
;;; refined lattice types for type inference
;;;-----------------------------------------------------------------------------
;; Singleton subtypes of <fixed-precision-integer>.
(%define-lattice-type fpi-zero
  (<fixed-precision-integer> singleton)
  (bottom) () 0)

(%define-lattice-type fpi-one
  (<fixed-precision-integer> singleton)
  (bottom) () 1)

(%define-lattice-type fpi-rest
  (<fixed-precision-integer>)
  (bottom))

(%define-lattice-type zero
  (fpi-zero)
  (bottom))

(%define-lattice-type one
  (fpi-one)
  (bottom))

;; Compound list for elements of type <fixed-precision-integer>.
(%define-lattice-type fpi-list
  (mono-list)
  (bottom) t)

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
