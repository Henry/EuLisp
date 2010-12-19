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
;;; Title: Syntax for eval
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Henry G. Weller
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule syntax-eval
  (syntax (syntax-0)
   import (level-1
           sx-obj1)
   export (eval))

(defmacro eval (x . mod)
  (let* ((current-module-name (module-name? (dynamic *actual-module*))))
    `(let ((eval-module-name (or ,@mod `,current-module-name)))
       (set-eval-module eval-module-name)
       (let ((res (eval/cm ,x)))
         (dynamic-setq *actual-module* (get-module `,current-module-name))
         res))))

;;;-----------------------------------------------------------------------------
)  ;; End of module syntax-eval
;;;-----------------------------------------------------------------------------
