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
;;; Title: defining all classes of the abstract syntax tree
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule sx-obj2
  (syntax (_macros
           _sx-obj0)
   import (i-level1
           sx-obj1)
   ;; generated accessors and predicates are automatiacally exported
   )

;;;-----------------------------------------------------------------------------
;;;  Variables
;;;-----------------------------------------------------------------------------
(def-syntax-obj <var> (<syntax-expr>) (name binding value used))
(def-syntax-obj <local-var> (<var>) ())
(def-syntax-obj <local-static-var> (<local-var>) (lambda captured))
(def-syntax-obj <global-var> (<var>) ())
(def-syntax-obj <global-static-var> (<global-var>) ())
(def-syntax-obj <setq> (<syntax-expr>) (binding obj))

;;;-----------------------------------------------------------------------------
;;;  Constants
;;;-----------------------------------------------------------------------------
(def-syntax-obj <const> (<syntax-obj>) (value))
(def-syntax-obj <named-const> (<const>) (name binding))
(def-syntax-obj <literal-const> (<const>) ())

;;;-----------------------------------------------------------------------------
)  ;; End of module sx-obj2
;;;-----------------------------------------------------------------------------
