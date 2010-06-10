;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: defining all classes of the abstract syntax tree
;;;-----------------------------------------------------------------------------
(defmodule sx-obj2
  (syntax (_macros _sx-obj0)
   import (i-level1 sx-obj1)
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
  )  ;; end of module
;;;-----------------------------------------------------------------------------
