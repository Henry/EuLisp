;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: foreign function interface
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule i-ffi
  (syntax (_macros)
   import (i-all)
   export (arg-converter-index
           res-converter-index
           arg-converter-as-C-type
           res-converter-as-C-type))

;;;-----------------------------------------------------------------------------
;;; Argument converters
;;;-----------------------------------------------------------------------------
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

;;;-----------------------------------------------------------------------------
;;; Result value converters
;;;-----------------------------------------------------------------------------
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

;;;-----------------------------------------------------------------------------
)  ;; End of module i-ffi
;;;-----------------------------------------------------------------------------
