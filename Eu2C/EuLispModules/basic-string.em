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
;;; Title: Basic string functionality
;;;  Authors: Ingo Mohr, E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule basic-string
  (import ((rename ((%class <class>))
                   %tail)
           mm-interface
           c-string-interface)
   syntax (%tail)
   export (duplicate-%string
           allocate-%string
           %string))

;; The following type and card descriptors for %string are the only ones which
;; are predefined and which cannot be changed
(%define-constant $%string-type-descriptor
  (%literal %unsigned-word-integer 2))

(%define-constant $%string-card-descriptor
  (%literal %unsigned-word-integer 2))

(%define-function (duplicate-%string %string)
  ((string %string))
  (strcpy (allocate-%string (strlen string)) string))

(%define-function (allocate-%string %string)
  ((length %signed-word-integer))
  (%cast %string (allocate-on-multiple-size-card
                  $%string-type-descriptor ; see the call of set-card-descriptor
                                           ; below
                  (%plus length #%i1))))

(%define-constant unique-address-for-gc-type-descriptor-of-%string
  (%literal %string () "*"))

(set-type-descriptor $%string-type-descriptor
                     (%cast %unsigned-word-integer
                            unique-address-for-gc-type-descriptor-of-%string)
                     (%function trace-nothing))

(set-card-descriptor $%string-card-descriptor
                     stms  ; card type single-type-multiple-size
                     #%i0  ; size not needed for multiple-size-card
                     $%string-type-descriptor);;multiple-size-card

;;;-----------------------------------------------------------------------------
)  ;; End of basic-string
;;;-----------------------------------------------------------------------------
