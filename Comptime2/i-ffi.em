;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Description: foreign function interface
;;; -----------------------------------------------------------------------
(defmodule i-ffi
  (syntax (_macros)
   import (i-all)
   export (arg-converter-index res-converter-index
           arg-converter-as-C-type res-converter-as-C-type))
;;; --------------------------------------------------------------------
;;; Argument converters
;;; --------------------------------------------------------------------
  (defconstant *argument-converters*
    #(<int> <character> <double> <string> <vector> <file-stream>
      <symbol> boolean ptr <int*> <double*> <string*>))
  (defconstant *argument-converter-C-types*
    #("int" "char" "double" "const char *" "void **" "int"
      "const char *" "int" "void *" "int *" "double *" "char **"))
  (defun arg-converter-index (x)
    (or (find x *argument-converters*)
        (ct-serious-warning 0 "bad defextern argument converter ~a" x)))
  (defun arg-converter-as-C-type (i)
    (vector-ref *argument-converter-C-types* i))
;;; --------------------------------------------------------------------
;;; Result value converters
;;; --------------------------------------------------------------------
  (defconstant *result-converters*
    #(<int> <character> <double> <string> <symbol> boolean ptr
      <int*> <double*> <string*> void))
  (defconstant *result-converter-C-types*
    #("int" "char" "double" "char *" "char *" "int" "void *"
      "int *" "double *" "char ** "void *""))
  (defun res-converter-index (x)
    (or (find x *result-converters*)
        (ct-serious-warning 0 "bad defextern result converter ~a" x)))
  (defun res-converter-as-C-type (i)
    (vector-ref *result-converter-C-types* i))
)  ; end of module
