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
;;; Title: Interface to the C math library
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule c-math
  (import (tail
           (only (<long*>)
                 integer-32)
           (only (<double-float>)
                 double-float-i))
   syntax (tail)
   c-import (<math.h>)
   export (%acos
           %asin
           %atan
           %atan2
           %cos
           %sin
           %tan
           %cosh
           %sinh
           %tanh
           %exp
           %log
           %log10
           %pow
           %sqrt
           %ceil
           %floor
           %fabs
           %ldexp
           %frexp
           %modf
           %fmod
           <double*>))

(%declare-external-class (<double*> <tail-class>)
  ()
  ()
  language C
  representation pointer-to-void
  type-identifier |double *|)

(%declare-external-function (%acos %double-float)
  ((d %double-float))
  language C
  external-name |acos|)

(%declare-external-function (%asin %double-float)
  ((d %double-float))
  language C
  external-name |asin|)
(%declare-external-function (%atan %double-float)
  ((d %double-float))
  language C
  external-name |atan|)

(%declare-external-function (%atan2 %double-float)
  ((d1 %double-float)
   (d2 %double-float))
  language C
  external-name |atan2|)
(%declare-external-function (%cos %double-float)
  ((d %double-float))
  language C
  external-name |cos|)

(%declare-external-function (%sin %double-float)
  ((d %double-float))
  language C
  external-name |sin|)

(%declare-external-function (%tan %double-float)
  ((d %double-float))
  language C
  external-name |tan|)

(%declare-external-function (%cosh %double-float)
  ((d %double-float))
  language C
  external-name |cosh|)

(%declare-external-function (%sinh %double-float)
  ((d %double-float))
  language C
  external-name |sinh|)

(%declare-external-function (%tanh %double-float)
  ((d %double-float))
  language C
  external-name |tanh|)

(%declare-external-function (%exp %double-float)
  ((d %double-float))
  language C
  external-name |exp|)

(%declare-external-function (%log %double-float)
  ((d %double-float))
  language C
  external-name |log|)

(%declare-external-function (%log10 %double-float)
  ((d %double-float))
  language C
  external-name |log10|)

(%declare-external-function (%pow %double-float)
  ((d1 %double-float)
   (d2 %double-float))
  language C
  external-name |pow|)

(%declare-external-function (%sqrt %double-float)
  ((d %double-float))
  language C
  external-name |sqrt|)

(%declare-external-function (%ceil %double-float)
  ((d %double-float))
  language C
  external-name |ceil|)
(%declare-external-function (%floor %double-float)
  ((d %double-float))
  language C
  external-name |floor|)

(%declare-external-function (%fabs %double-float)
  ((d %double-float))
  language C
  external-name |fabs|)

(%declare-external-function (%ldexp %double-float)
  ((d1 %double-float)
   (n %signed-word-integer))
  language C
  external-name |ldexp|)

(%declare-external-function (%frexp %double-float)
  ((d1 %double-float)
   (n <long*>))
  language C
  external-name |frexp|)

(%declare-external-function (%modf %double-float)
  ((d1 %double-float)
   (n <double*>))
  language C
  external-name |modf|)
(%declare-external-function (%fmod %double-float)
  ((d1 %double-float)
   (d2 %double-float))
  language C
  external-name |fmod|)

;;;-----------------------------------------------------------------------------
)  ;; End of module c-math
;;;-----------------------------------------------------------------------------
