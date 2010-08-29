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
;;;  Title: 
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule letcc

  (import (apply-level-2
           basic-symbol)

   syntax (apply-level-2)

   c-import (<setjmp.h>  ; needed to get setjmp and longjmp, which must be
             ;; available for code generation
             <stdlib.h>) ; to get NULL

   export (unwind
           stop-unwind-before
           continue-at
           top-dynamic
           global-dynamic
           letcc-result

           unwind-continue

           <dynamic>
           make-dynamic
           %dynamic
           %dynamic-setq
           )
   )

(%define-variable unwind %pjmpbuf)
(%define-variable stop-unwind-before %pjmpbuf)
(%define-variable continue-at %pjmpbuf)
(%define-variable top-dynamic <dynamic>) ; holds the list of current dynamic bindings
(%define-variable global-dynamic <dynamic>) ; holds the list of global dynamic bindings
(%define-variable letcc-result <object>)

(%declare-external-function (get-jmpbuf %jmpbuf)
  ((ptr-to-jmpbuf %pjmpbuf))
  external-name |GET_JMPBUF|
  ;; the language is Lisp because get-jmpbuf does only dereferencing which
  ;; should not corrupt the NIL-register
  language lisp)

(%declare-external-variable nullptr %pjmpbuf
  external-name |NULL|
  language c)

(%define-function (unwind-continue %void) ((unwinder %pjmpbuf))
  (if (%eq unwinder stop-unwind-before)
      (%longjmp (get-jmpbuf continue-at) #%i1)
    (if (%eq unwinder nullptr)
        #%i0
      (%longjmp (get-jmpbuf unwinder) #%i1))))

(%define-standard-class (<dynamic> <class>)
  <object>
  ((name type <symbol>
         reader dynamic-name
         keyword name)
   (value type <object>
          reader dynamic-value
          writer set-dynamic-value
          keyword value)
   (next type <object> ; <dynamic> isn't possible yet because
         ;; compute-representation can't handle recursive definitions
         reader next-dynamic
         keyword next))
  constructor (make-dynamic name value next)
  allocation multiple-type-card
  representation pointer-to-struct)

(%define-function (%dynamic <object>) ((name <symbol>))
  (dynamic-value (get-dynamic name top-dynamic)))

(%define-function (%dynamic-setq <object>) ((name <symbol>) (value <object>))
  (set-dynamic-value (get-dynamic name top-dynamic) value))

(%define-function (get-dynamic <dynamic>) ((name <symbol>) (var <dynamic>))
  ;; a null test is needed to handle the errorneous case that no special binding is found
  ;; which can only occur during module initialization ( a dynamic binding is used before
  ;; it is initialized)
  (if (%eq (dynamic-name var) name)
      var
    (get-dynamic name (next-dynamic var))))

(%define-function (initialize-global-dynamic %void)
  ((name <symbol>) (value <object>))
  (setq global-dynamic (make-dynamic name value global-dynamic))
  (setq top-dynamic global-dynamic))

(%annotate-function initialize-global-dynamic
  is-special-function initialize-global-dynamic)

) ;end letcc
