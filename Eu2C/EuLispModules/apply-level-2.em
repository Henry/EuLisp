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
;;;  Title: The most basic dynamically loaded things for the compiler
;;;  Description:
;;    This module contains all basic stuff for the compiler which is not hard-wired
;;    but is needed for
;;    - defining classes
;;    - ???
;;    This module is loaded explicitely by the compiler before any other module is
;;    required. The following things are available for this module and the modules it
;;    uses:
;;    - definition of simple functions
;;    - constant and variable definitions
;;    - definition of external functions, constants and variables
;;    - predefined classes of TAIL and standard literals
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------


(defmodule apply-level-2
  (import (apply-level-1
           basic-condition
           ti-sys-signatures   ; provides signatures for predefined tail-functions
           basic-list
           basic-syntax
           basic-number; *UK* 22.09.93
           string-i
           tail-introspection
           basic-std-gf
           function)
   syntax (apply-level-1
           basic-syntax)
   expose (apply-level-1
           basic-compare
           basic-condition
           basic-list
           basic-syntax
           basic-std-gf
           string-i
           tail-introspection
           basic-number
           closure)) ; *hf* 18.08

;;;-----------------------------------------------------------------------------
;;; literal expanders for pair and null
;;;-----------------------------------------------------------------------------
;;; the literal expanders for pair and null cannot be defined earlier, because
;;; basic-syntax is needed and basic symtax needs basic-list

(%define-literal-expansion
  null
  `(%literal ,<null>))

(%define-literal-expansion
  pair
  `(%literal ,<cons>
             car ,car
             cdr ,cdr))

(%define-literal-expansion string
  `(%literal ,<string> characters (%literal ,%string () ,elements))
  )

#+(:int :big)
(progn
  (%declare-external-function (auxplus <integer>)
    ((s1 <integer>) (s2 <integer>))
    ;;for annotation only
    )

  (%annotate-function auxplus interpreter +)

  (%define-literal-expansion
    integer
    `(%literal ,<int>
               ,(auxplus (auxplus value value) 1)))
  )

#-(:int :big)
(%define-literal-expansion
  integer
  `(%literal ,<int>
             ,value))


;;;-----------------------------------------------------------------------------
;;; providing some special objects to the compiler
;;;-----------------------------------------------------------------------------

(%annotate-function %instance-of-p is-special-function instance-of-p)
(%annotate-function %class-of is-special-function class-of)
(%annotate-function %vector-class-instance-size is-special-function vector-class-instance-size)

(%annotate-class <list> is-special-class <list>)
(%annotate-class <null> is-special-class <null>)
(%annotate-class <int> is-special-class <fpi>)
(%annotate-class <function> is-special-class <function>)

(%annotate-function cons is-special-function cons)
(%annotate-function null? is-special-function null?)
(%annotate-function eq is-special-function eq)

(%annotate-function no-applicable-method-error is-special-function no-applicable-method-error)
(%annotate-function call-next-method is-special-function call-next-method)
(%annotate-function next-method? is-special-function next-method?)
(%annotate-function typecheck is-special-function typecheck)

)
