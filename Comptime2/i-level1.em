;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: binds together the almost-everywhere-used modules
;;;    And we don't need generic arithmetic/reversal.
;;;-----------------------------------------------------------------------------
(defmodule i-level1
  (import ((rename ((reverse-list reverse)) boot))
   expose ((except (+ - * / % = <  reverse) level1)
           aux-table)
   export (+ - * / % = <  reverse))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
