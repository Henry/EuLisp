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
;;;-----------------------------------------------------------------------------

(defmodule integer-32
  (import (tail)
   syntax (tail)
   export (<integer-32>  <long*> make-i32 i32 setf-i32))


(%define-tail-class (<integer-32>  <tail-class>)
  ((i32 type %signed-word-integer
        reader i32
        writer setf-i32
        keyword value))
  constructor (make-i32 value)
  representation pointer-to-struct
  allocation multiple-type-card)



(%declare-external-class (<long*> <tail-class>)
  ()
  ()
  language C
  representation pointer-to-void
  type-identifier |long *|)
)
