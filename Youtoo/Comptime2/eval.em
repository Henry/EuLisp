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
;;; Title: Eval
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Henry G. Weller
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule eval
  (syntax (_macros
           syntax-eval)
   import ((except (eval)
                   i-rep)
           p-env)
   export (set-eval-module
           eval/cm
           get-module
           macroexpand-1
           macroexpand))

;;;-----------------------------------------------------------------------------
)  ;; End of module eval
;;;-----------------------------------------------------------------------------
