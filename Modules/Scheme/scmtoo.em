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
;;; Title: Scheme interpreter/compiler entry point
;;;  Library: scheme
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule scmtoo
  (import (scheme
           scheme0
           scmtoo0
           (only (main
                  *redefine-imported-bindings*)
                 eval)
           (only (*argv*
                  push-dynamic-variable
                  dynamic-variable-ref
                  pop-dynamic-variables)
                 level1))
   export (*redefine-imported-bindings*
           push-dynamic-variable
           dynamic-variable-ref
           pop-dynamic-variables))

(main *argv*)

;;;-----------------------------------------------------------------------------
)  ;; End of module scmtoo
;;;-----------------------------------------------------------------------------
