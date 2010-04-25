;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: foreign function test
;;;  Compilation
;;    cc -c eul-ffi2.c
;;    youtoo ffi2 -l level1 -fff eul-ffi2
;;;-----------------------------------------------------------------------------
(defmodule ffi2
  (syntax (macros)
   import (level1))

;;;-----------------------------------------------------------------------------
;;; External functions using C pointers
;;;-----------------------------------------------------------------------------
  (defextern ext_foo_int (<int*>) <int>)
  (defextern ext_foo_double (<double*>) <double>)
  (defextern ext_foo_string (<string*>) <string>)
  (defextern ext_foo_int2 () <int*>)
  (defextern ext_foo_double2 () <double*>)
  (defextern ext_foo_string2 () <string*>)

;;;-----------------------------------------------------------------------------
;;; Example calls
;;;-----------------------------------------------------------------------------
  (let ((x (convert 3 <int*>)))
    (ext_foo_int x)
    (print (convert x <int>)))
  (let ((x (convert 3.0 <double*>)))
    (ext_foo_double x)
    (print (convert x <double>)))
  (let ((x (convert "abc" <string*>)))
    (ext_foo_string x)
    (print (convert x <string>)))
  (let ((x (ext_foo_int2)))
    (print (convert x <int>)))
  (let ((x (ext_foo_double2)))
    (print (convert x <double>)))
  (let ((x (ext_foo_string2)))
    (print (convert x <string>)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
