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
;;; Title: generic functions of default module compare
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;    The default generic functions with special characters in its name are defined
;;    with only character names to provide a better readability at the C level. The
;;    EuLisp names are provided by the special interface module 'compare-generic'.
;;;  Requires:
;;;  Problems:
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule compare-generic-i
  (import
   (eulisp-kernel
    (only (<number>) number-i))

   syntax
   (eulisp-kernel)

   export
   (binary-lt binary-eq equal)
   )

;;;-----------------------------------------------------------------------------
;;;
;;;-----------------------------------------------------------------------------

(defgeneric binary-lt ((n1 <object>) (n2 <object>)))

(defgeneric binary-eq ((n1 <number>) (n2 <number>)))

(defgeneric equal (o1 o2))

;;;-----------------------------------------------------------------------------
;;; type schemes for type inference are defined in compare-generic
;;;-----------------------------------------------------------------------------

)
