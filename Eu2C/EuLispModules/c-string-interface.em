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
;;;  Title: interface to c-strings
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule c-string-interface

  (import (%tail)
   syntax (%tail)
   export (strcmp strdup strlen strcpy strncpy)
   c-import (<string.h>))

(%declare-external-function (strcmp %signed-word-integer)
  ((s1 %string)
   (s2 %string))
  external-name |strcmp|
  language C)

(%declare-external-function (strdup %string)
  ((s %string))
  external-name |strdup|
  language C)

(%declare-external-function (strlen %signed-word-integer)
  ((s %string))
  external-name |strlen|
  language C)

(%declare-external-function (strcpy %string)
  ((s1 %string)
   (s2 %string))
  external-name |strcpy|
  language C)
(%declare-external-function (strncpy %string)
  ((s1 %string)
   (s2 %string)
   (n %signed-word-integer))
  external-name |strncpy|
  language C)
)
