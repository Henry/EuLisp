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
;;;  Title: basic stuff for lists
;;;  Description:
;;    This module contains all definitions for lists which are needed by other
;;    basic parts of the runtime environment provided by the module tail. The
;;    modules list, cons and null must use these definitions and should provide
;;    additional things.
;;;  Notes:
;;    This module is needed for apply-level-1 and therefore some restrictions
;;    must be considered, e.g. class definitions without predicates.
;;;  Authors: Ingo Mohr, E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule basic-list-0
  (import (ti-sys-signatures ;; this allows declaration of signatures
           apply-level-1
           basic-compare)
   syntax (apply-level-1)
   export (<list>
           <cons>
           <null>
           car
           cdr
           cons
           null?
           list
           %list-length
           %member))

;;;-----------------------------------------------------------------------------
;;; classes: <list>, <cons> and <null>
;;;-----------------------------------------------------------------------------
(%define-abstract-class (<list> <abstract-class>)
  <object>
  ())

(%define-standard-class (<cons> <class>)
  <list>
  ((car type <object>
        accessor car
        keyword car
        )
   (cdr type <object>
        accessor cdr
        keyword cdr
        ))
  constructor (cons car cdr)
  allocation single-card
  representation pointer-to-struct)

(%declare-external-class (<long*> <tail-class>) ()
  ()
  representation pointer-to-void
  language c
  type-identifier |long*|)

(%define-standard-class (<null> <class>)
  <list>
  ;;((null-slot type %signed-word-integer))
  ((null-slot type <long*>))
  ;;allocation multiple-type-card
  ;;representation pointer-to-struct
  representation direct
  direct-super-lattice-types (singleton))

;; the code identifier for the C-level must be set explicitely because it is
;; used explicitely in the header file eu2c-sys.h to put () into a register
(%annotate-class <null> code-identifier |c__null__eulisp0|)

;;;-----------------------------------------------------------------------------
;;; predicates (automatic generation at apply-level-1 isn't possible)
;;;-----------------------------------------------------------------------------
(defun null? (object)
  (if (%eq (%cast %signed-word-integer object)
           (%cast %signed-word-integer ()))
      t
    nil))

;;;-----------------------------------------------------------------------------
;;; length-functions
;;;-----------------------------------------------------------------------------
(%define-function (%list-length %signed-word-integer)
  ((arg <list>))
  (if arg (%plus (%list-length (cdr arg)) #%i1) #%i0))

;;;-----------------------------------------------------------------------------
;;; member-function
;;;-----------------------------------------------------------------------------
(%define-function (%member <object>)
  ((element <object>)
   (li <list>))
  (if (%eq (%cast %unsigned-word-integer li)
           (%cast %unsigned-word-integer ()))
      ()
    (if (%eq (%cast %unsigned-word-integer element)
             (%cast %unsigned-word-integer (car li)))
        element
      (%member element (cdr li)))))

;;;-----------------------------------------------------------------------------
;;; list
;;;-----------------------------------------------------------------------------
(defun list x x)
(%annotate-function list interpreter list)

;;;-----------------------------------------------------------------------------
;;; strategic lattice types for monomorphic/polymorphic lists
;;;-----------------------------------------------------------------------------
(%define-lattice-type mono-list (<cons>) (bottom) t)
(%define-lattice-type poly-list (<cons>) (bottom) t)

;; The strategic type mono-list is also specialized in basic-number.am.
(%define-lattice-type cons-list (mono-list) (bottom) t)
(%define-lattice-type sy-list (mono-list) (bottom) t)

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------
(%annotate-function
  car new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <cons>)))))

(%annotate-function
  cdr new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? poly-list)))
   ((var0 var1)
    ((var var0) (atom? mono-list))
    ((var var1) (var var0)))
   ((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? mono-list)))))

(%annotate-function
  cons new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <cons>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <object>)))))

(%annotate-function
  null? new-signature
  (((var0 var1)
    ((var var0) (atom? (and <object> (not <null>))))
    ((var var1) (atom? <null>)))
   ((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (and <object> (not <null>)))))))

(%annotate-function
  list new-signature
  (((var0 var1)
    ((var var0) (atom? <list>))
    ((var var1) (var var0)))))

;; see also ti-sys-signatures.am
(%annotate-function
  %eq renew-signature
  (((var0 var1 var2)
    ((var var0) (atom? (not %false)))
    ((var var1) (atom? <null>))
    ((var var2) (var var1)))
   ((var0 var1 var2)
    ((var var0) (atom? top))
    ((var var1) (atom? (not <null>)))
    ((var var2) (atom? (not <null>))))
   ((var0 var1 var2)
    ((var var0) (atom? %false))
    ((var var1) (atom? (not <null>)))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? %false))
    ((var var1) (atom? <null>))
    ((var var2) (atom? (not <null>))))))

;; see also ti-sys-signatures.am
(%annotate-function
  %neq renew-signature
  (((var0 var1 var2)
    ((var var0) (atom? %false))
    ((var var1) (atom? <null>))
    ((var var2) (var var1)))
   ((var0 var1 var2)
    ((var var0) (atom? top))
    ((var var1) (atom? (not <null>)))
    ((var var2) (atom? (not <null>))))
   ((var0 var1 var2)
    ((var var0) (atom? (not %false)))
    ((var var1) (atom? (not <null>)))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? (not %false)))
    ((var var1) (atom? <null>))
    ((var var2) (atom? (not <null>))))))

;; Defined in basic-compare.am but there is no class <null> available.

(%annotate-function
  eq new-signature
  (((var0 var1 var2)
    ((var var0) (atom? (and <object> (not <null>))))
    ((var var1) (atom? <object>))
    ((var var2) (var var1)))
   ((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <object>)))))

;;;-----------------------------------------------------------------------------
) ; end of module basic-list-0
;;;-----------------------------------------------------------------------------
