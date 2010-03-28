;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Description: defining all classes of the abstract syntax tree
;;; -----------------------------------------------------------------------
(defmodule sx-obj1
  (syntax (_macros _sx-obj0)
   import (i-level1)
   ;; generated accessors and predicates are automatically exported
   )
;;; ----------------------------------------------------------------------
;;;  Abstract root
;;; ----------------------------------------------------------------------
  (def-syntax-obj <syntax-obj> () ())
;;; ----------------------------------------------------------------------
;;;  Defined objects and expressions
;;; ----------------------------------------------------------------------
  (def-syntax-obj <syntax-def> (<syntax-obj>) ())
  (def-syntax-obj <syntax-expr> (<syntax-obj>) (encl-lambda))
  (defmethod initialize ((obj <syntax-expr>) (init-list <list>))
    (call-next-method)
    (syntax-expr-encl-lambda! obj (dynamic *encl-lambda*))
    obj)
;;; ----------------------------------------------------------------------
;;;  Modules, bindings
;;; ----------------------------------------------------------------------
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
)  ; end of module
