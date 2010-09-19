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
;;; Title: Example 'test-introspection'
;;;  Description:
;;    Basic tests of class introspection
;;;  Authors: Henry G. Weller
;;;-----------------------------------------------------------------------------
(defmodule test-introspection
  (import (level-0
           object-1)
   syntax (level-0
           object-1))

(print (class-name (car (cdr (class-precedence-list <list>)))) nl)
(print (size (class-name <list>)) nl)
(print (class-precedence-list <list>) nl)
(print <list> nl)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
