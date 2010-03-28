;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: fortran
;;;  Authors: Andreas Kind
;;;  Description: Fortran interoperability
;;;  Compilation: 
;;;   ../youtoo -c test2 -l level1
;;;   f77 -c eul-f2.f
;;;   ../youtoo test2 -l level1 -fff eul-f2 -recompile
;;; -----------------------------------------------------------------------
(defmodule test2
  (syntax (macros)
   import (level1)
   export (bar))
;;; --------------------------------------------------------------------
;;; In-call from Forran
;;; --------------------------------------------------------------------
  (defun bar (x y)
    (+ x y))
;;; --------------------------------------------------------------------
;;; Out-call to Fortran
;;; --------------------------------------------------------------------
  (defextern foo (<int*> <double*>) <double>)
  (print (foo (convert 42 <int*)1 (convert 1.0 <double*>)))
)  ;; end of module
