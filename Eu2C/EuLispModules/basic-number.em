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
;;; Title: basic definitions to realize number classes
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule basic-number
  (import (apply-level-1
           ti-sys-signatures)
   syntax (%tail)
   export (<number>
           <integer>
           <int>
           make-fpint
           make-swi))

(%define-abstract-class (<number> <abstract-class>)
  <object>
  ())

(%define-abstract-class (<integer> <abstract-class>)
  <number>
  ())

(%define-standard-class (<int> <class>)
  <integer>
  ((fpint type %signed-word-integer
          reader fpint))
  ;;constructor (make-fpint fpint) will be handmaded
  representation direct)


#+(:int :big)
(%define-function (make-fpint <int>)
  ((val %signed-word-integer))
  (%cast <int>
         (%or (%plus val val) #%i1)))

#+(:int :big)
(%define-function (make-swi %signed-word-integer)
  ((val <int>))
  (%ashiftr (fpint val) #%I1))


#-(:int :big)
(%define-function (make-fpint <int>)
  ((val %signed-word-integer))
  (%cast <int>
         (%ashiftr (%lshiftl val #%I16) #%I16)))

#-(:int :big)
(%define-function (make-swi %signed-word-integer)
  ((val <int>))
  (fpint val))


;;;-----------------------------------------------------------------------------
;;; refined lattice types for type inference
;;;-----------------------------------------------------------------------------
;; Singleton subtypes of <int>.
(%define-lattice-type fpi-zero
  (<int> singleton)
  (bottom) () 0)

(%define-lattice-type fpi-one
  (<int> singleton)
  (bottom) () 1)

(%define-lattice-type fpi-rest
  (<int>)
  (bottom))

(%define-lattice-type zero
  (fpi-zero)
  (bottom))

(%define-lattice-type one
  (fpi-one)
  (bottom))

;; Compound list for elements of type <int>.
(%define-lattice-type fpi-list
  (mono-list)
  (bottom) t)

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
