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
;;;  Title: 
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: 
;;;-----------------------------------------------------------------------------

(defmodule unix-interface
  (import (tail)
   syntax (tail)
   export (command-line-arguments)
   )

(%define-tail-class (<vector-of-strings> <tail-class>)
  ((element type %string reader string-n)
   (length type %unsigned-word-integer))
  representation pointer-to-vector)

(%declare-external-variable command-line-size %signed-word-integer
  language C
  external-name |command_line_length|)

(%declare-external-variable command-line <vector-of-strings>
  language C
  external-name |command_line|)

(defun command-line-arguments ()
  (make-argument-list #%i0 ()))

(%define-function (make-argument-list <cons>)
  ((n %signed-word-integer)
   (li <list>))
  (if (%eq n command-line-size)
      li
    (cons (make-string (string-n command-line
                                 (%cast %unsigned-word-integer n)))
          (make-argument-list (%plus n #%i1) li))))

)
