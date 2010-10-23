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
           <fpi>
           make-fpi
           make-swi))

(%define-abstract-class (<number> <abstract-class>)
  <object>
  ())

(%define-abstract-class (<integer> <abstract-class>)
  <number>
  ())

(%define-standard-class (<fpi> <class>)
  <integer>
  ((fpi type %signed-word-integer
          reader fpi))
  ;;constructor (make-fpi fpi) will be handmaded
  representation direct)


#+(:int :big)
(%define-function (make-fpi <fpi>)
  ((val %signed-word-integer))
  (%cast <fpi>
         (%or (%plus val val) #%i1)))

#+(:int :big)
(%define-function (make-swi %signed-word-integer)
  ((val <fpi>))
  (%ashiftr (fpi val) #%I1))


#-(:int :big)
(%define-function (make-fpi <fpi>)
  ((val %signed-word-integer))
  (%cast <fpi>
         (%ashiftr (%lshiftl val #%I16) #%I16)))

#-(:int :big)
(%define-function (make-swi %signed-word-integer)
  ((val <fpi>))
  (fpi val))


;;;-----------------------------------------------------------------------------
;;; refined lattice types for type inference
;;;-----------------------------------------------------------------------------
;; Singleton subtypes of <fpi>.
(%define-lattice-type fpi-zero
  (<fpi> singleton)
  (bottom) () 0)

(%define-lattice-type fpi-one
  (<fpi> singleton)
  (bottom) () 1)

(%define-lattice-type fpi-rest
  (<fpi>)
  (bottom))

(%define-lattice-type zero
  (fpi-zero)
  (bottom))

(%define-lattice-type one
  (fpi-one)
  (bottom))

;; Compound list for elements of type <fpi>.
(%define-lattice-type fpi-list
  (mono-list)
  (bottom) t)

;;;-----------------------------------------------------------------------------
)  ;; End of module basic-number
;;;-----------------------------------------------------------------------------
