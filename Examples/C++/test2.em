;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: cc
;;;  Authors: Andreas Kind
;;; Description: C++ interoperability
;;;  Compilation
;;   youtoo -c test2 -l level1
;;   gcc -c -I../Gc eul-cc2.c
;;   g++ -c eul-cc3.cc
;;   youtoo test2 -l level1 -fff eul-cc2 -fff eul-cc3 -recompile
;;;-----------------------------------------------------------------------------
(defmodule test2
  (syntax (macros)
   import (level1)
   export (bar))

;;;-----------------------------------------------------------------------------
;;; In-call from C++
;;;-----------------------------------------------------------------------------
  (defun bar (str i)
    (string-ref str i))

;;;-----------------------------------------------------------------------------
;;; Out-call to C++
;;;-----------------------------------------------------------------------------
  (defextern foo (<string> <int>) <character>)
  (print (foo "Hello world!" 4))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
