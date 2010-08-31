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
;;;  Title: generic functions of default module number
;;;  Description:
;;    Module contains all generic functions defined for subclasses of class
;;    <number>
;;;  Notes:
;;    The default generic functions with special characters in its name are
;;    defined with only character names to provide a better readability at the C
;;    level. The EuLisp names are provided by a special interface module
;;    'number-generic'.
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule number-generic-i
  (import (eulisp-kernel
           (only (<number>)
                 number-i))
   syntax (eulisp-kernel)
   export (binary-plus
           binary-minus
           binary-div
           binary-mult
           binary-rem
           binary-mod
           binary-gcd
           binary-lcm
           negate
           zero?))


(defgeneric binary-plus ((n1 <number>) (n2 <number>)))

(defgeneric binary-minus ((n1 <number>) (n2 <number>)))

(defgeneric binary-div ((n1 <number>) (n2 <number>)))

(defgeneric binary-mult ((n1 <number>) (n2 <number>)))

(defgeneric binary-rem ((n1 <number>) (n2 <number>)))

(defgeneric binary-mod ((n1 <number>) (n2 <number>)))

(defgeneric binary-gcd ((n1 <number>) (n2 <number>)))

(defgeneric binary-lcm ((n1 <number>) (n2 <number>)))

(defgeneric negate ((n <number>)))

(defgeneric zero? ((n <number>)))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
