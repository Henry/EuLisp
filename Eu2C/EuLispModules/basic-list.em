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
;;    basic-syntax provides all stuff which is needed to write simple macros and to
;;    use quasiquote.
;;;  Documentation:
;;;  Notes:
;;    append and cons? are defined in this module and should be imported from here.
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------
(defmodule basic-list
  (import (%tail
           ti-sys-signatures;; this allows declaration of signatures
           basic-list-0
           basic-compare
           tail-introspection
           (only (<int>)
                 basic-number)
           apply-level-1)
   syntax (%tail)
   expose (basic-list-0)
   export (%pair-length
           eq;;; basic-compare
           cons? append
           t))

;;;-----------------------------------------------------------------------------
;;; append
;;;-----------------------------------------------------------------------------
(defun cons? (object)
  (%instance-of-p object <cons>))

(defun append (list1 list2)
  (if (cons? list1)
      (cons (car list1)
            (append (cdr list1) list2))
    list2))

;;;-----------------------------------------------------------------------------
;;; length-functions
;;;-----------------------------------------------------------------------------

(%define-function (%pair-length %signed-word-integer )
  ((l <list>))
  (if (cons? l) (%plus #%i1 (%pair-length (cdr l)))
    #%i0))

;;;-----------------------------------------------------------------------------
;;; interpreter annotations
;;;-----------------------------------------------------------------------------

(%annotate-function car interpreter car)
(%annotate-function cdr interpreter cdr)
(%annotate-function cons interpreter cons)
(%annotate-function append interpreter append)
(%annotate-function eq interpreter eq)
(%annotate-function null? interpreter null?)

;;;-----------------------------------------------------------------------------
;;; type schemes for type inference
;;;-----------------------------------------------------------------------------

(%annotate-function
  cons? new-signature
  (((var0 var1)
    ((var var0) (atom? (not <null>)))
    ((var var1) (atom? <cons>)))
   ((var0 var1)
    ((var var0) (atom? <null>))
    ((var var1) (atom? (not <cons>))))))

(%annotate-function
  append new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <null>))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? <cons>))
    ((var var1) (atom? <cons>))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? <cons>))
    ((var var1) (atom? <null>))
    ((var var2) (atom? <cons>)))
   ((var0 var1 var2)
    ((var var0) (atom? <cons>))
    ((var var1) (atom? <cons>))
    ((var var2) (atom? <cons>)))))

;; Redefinition of the type scheme for compound types;
;; basic-number.am should be loaded before!
(%annotate-function
  car comp-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? poly-list)))
   ((var0 var1)
    ((var var0) (atom? <int>))
    ((var var1) (atom? fpi-list)))))

;; Redefinition of the type scheme for compound types;
;; basic-number.am should be loaded before!
(%annotate-function
  cons comp-signature
  (((var0 var1 var2)
    ((var var0) (atom? fpi-list))
    ((var var1) (atom? <int>))
    ((var var2) (atom? (or <null> fpi-list))))
   ((var0 var1 var2)
    ((var var0) (atom? poly-list))
    ((var var1) (atom? (not <int>)))
    ((var var2) (atom? <object>)))
   ((var0 var1 var2)
    ((var var0) (atom? poly-list))
    ((var var1) (atom? <object>))
    ((var var2) (atom? (and (not <null>) (not fpi-list)))))))


) ;end of module basic-list
