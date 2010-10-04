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
;;; Title: Interface to the C stdio library
;;;  Authors: Rainer Rosenmuller
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule c-stdio
  (import (tail
           (only (<double*>)
                 c-math))
   syntax (tail)
   c-import (<stdio.h>)
   export (FILE*
           c-stdin
           c-stdout
           c-stderr
           EOF
           fgetc
           fflush
           ungetc
           putc
           fprintf-3
           sprintf-3
           fscanf-3
           sscanf-3
           sscanf-3-double
           open-fd
           close-fd
           fseek
           ftell
           fprintf-3-string
           fprintf-3-double
           sprintf-3-double))


(%declare-external-class (FILE* <tail-class>)
  ()
  ()
  language C
  representation pointer-to-void
  type-identifier |FILE *|)


(%declare-external-variable c-stdin FILE*
  language c
  external-name |stdin|)

(%declare-external-variable c-stdout FILE*
  language c
  external-name |stdout|)

(%declare-external-variable c-stderr FILE*
  language c
  external-name |stderr|)


(%declare-external-variable EOF %signed-word-integer
  language C
  external-name |EOF|) ; from stdio.h of c !


(%declare-external-function (fgetc %signed-word-integer)
  ((arg1 FILE*))
  language c
  external-name |fgetc|)

(%declare-external-function (fflush %signed-word-integer)
  ((arg1 FILE*))
  language c
  external-name |fflush|)

(%declare-external-function (fseek %signed-word-integer)
  ((arg1 FILE*)
   (arg2 %signed-word-integer)
   (arg3 %signed-word-integer))
  language c
  external-name |fseek|)

(%declare-external-function (ftell %signed-word-integer)
  ((arg1 FILE* ))
  language c
  external-name |ftell|)

(%declare-external-function (ungetc %signed-word-integer)
  ((arg1 %signed-word-integer)
   (arg2 FILE*)) ;;%unsigned-word-integer))
  language c
  external-name |ungetc|)


(%declare-external-function (putc %signed-word-integer)
  ((arg1 %signed-word-integer)
   (arg2 FILE*))
  language c
  external-name |fputc|) ;because putc is a macro and results in type error

(%declare-external-function (fprintf-3 %signed-word-integer)
  ((arg1 FILE*)
   (arg2 %string)
   (arg3 %signed-word-integer))
  language c
  external-name |fprintf|)


(%declare-external-function (fprintf-3-string %signed-word-integer)
  ((arg1 FILE*)
   (arg2 %string)
   (arg3 %string))
  language c
  external-name |fprintf|)


(%declare-external-function (fprintf-3-double %signed-word-integer)
  ((arg1 FILE*)
   (arg2 %string)
   (arg3 %double-float))
  language c
  external-name |fprintf|)


(%declare-external-function (sprintf-3 %signed-word-integer)
  ((arg1 %string)
   (arg2 %string)
   (arg3 %signed-word-integer))
  language c
  external-name |sprintf|)

(%declare-external-function (sprintf-3-double %signed-word-integer)
  ((arg1 %string)
   (arg2 %string)
   (arg3 %double-float))
  language c
  external-name |sprintf|)

(%declare-external-function (fscanf-3 %signed-word-integer)
  ((arg1 FILE*)
   (arg2 %string)
   (arg3 %unsigned-word-integer))
  language c
  external-name |fscanf|)

(%declare-external-function (sscanf-3 %signed-word-integer)
  ((arg1 %string)
   (arg2 %string)
   (arg3 %unsigned-word-integer))
  language c
  external-name |sscanf|)

(%declare-external-function (sscanf-3-double %signed-word-integer)
  ((arg1 %string)
   (arg2 %string)
   (arg3 <double*>))
  language c
  external-name |sscanf|)

(%declare-external-function (open-fd FILE*)
  ((fname %string)
   (mode %string))
  language C
  external-name |fopen|
  )

(%declare-external-function (close-fd %signed-word-integer)
  ((fd FILE*))
  language C
  external-name |fclose|
  )

;;;-----------------------------------------------------------------------------
)  ;; End of module c-stdio
;;;-----------------------------------------------------------------------------
