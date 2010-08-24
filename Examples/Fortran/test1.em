;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: cc
;;;  Authors: Andreas Kind
;;; Description: C++ interoperability
;;;  Compilation
;;   f77 -c eul-f1.f
;;   youtoo test1 -l level1 -fff eul-f1
;;;-----------------------------------------------------------------------------
(defmodule test1
  (syntax (macros)
   import (level1))

;;;-----------------------------------------------------------------------------
;;; Out-call to Fortran
;;; Future: (defextern ... language: fortran) and no ref types
;;;-----------------------------------------------------------------------------
(defextern foo (<int*> <double*>) <double> "foo_")
(print (foo (convert 42 <int*>) (convert 1.0 <double*>)))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
