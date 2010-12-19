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

(defmodule sx-obj1
  (syntax (_syntax-1
           _sx-obj0)
   import (i-level-1)
   ;; generated accessors and predicates are automatically exported
   )

;;;-----------------------------------------------------------------------------
;;;  Abstract root
;;;-----------------------------------------------------------------------------
(def-syntax-obj <syntax-obj> () ())

;;;-----------------------------------------------------------------------------
;;;  Defined objects and expressions
;;;-----------------------------------------------------------------------------
(def-syntax-obj <syntax-def> (<syntax-obj>) ())

(def-syntax-obj <syntax-expr> (<syntax-obj>) (encl-lambda))

(defmethod initialize ((obj <syntax-expr>) (init-list <list>))
  (call-next-method)
  (syntax-expr-encl-lambda! obj (dynamic *encl-lambda*))
  obj)

;;;-----------------------------------------------------------------------------
;;;  Modules, bindings
;;;-----------------------------------------------------------------------------
(def-syntax-obj <binding> (<syntax-obj>)
                (local-name module immutable imported obj local-index info))

(def-syntax-obj <interface-binding> (<binding>) ())

(def-syntax-obj <module> (<syntax-obj>)
                (name load-dir c-module-name
                      binding-vector-size max-binding-vector-size
                      lexical-env external-env syntax-env
                      named-constants static-variables inlined-lambdas inlined-setters
                      named-lambdas anonymous-lambdas foreign-functions
                      top-level-forms lexical-binding-refs local-literals
                      used-module-names used-syntax-modules all-used-module-names
                      interactive-lexical-env))

;;;-----------------------------------------------------------------------------
)  ;; End of module sx-obj1
;;;-----------------------------------------------------------------------------
