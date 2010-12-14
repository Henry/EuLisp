;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp debugging helper macros
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule debugging
  (syntax (syntax)
   import (root
           telos)
   export (debugging
           dprint))

(deflocal debugging ())

(defmacro dprint (x)
  `(if debugging
       (print ,x nl)
     ()))

;;;-----------------------------------------------------------------------------
)  ;; End of module debugging
;;;-----------------------------------------------------------------------------
