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
;;; Title: String class and literal expander, no predicate
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule string-i
  (import (apply-level-1)
   syntax (%tail)
   export (<string>
           string-pointer
           make-string))

(%define-standard-class (<string> <class>)
  <object>
  ((characters
    type %string
    reader string-pointer
    keyword characters))
  representation pointer-to-struct
  allocation multiple-type-card
  constructor (make-string characters))

(defgeneric (converter <string>) (object))

;;;-----------------------------------------------------------------------------
)  ;; End of module string-i
;;;-----------------------------------------------------------------------------
