;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: scheme
;;;  Authors: Andreas Kind
;;;  Description: Scheme interpreter/compiler entry point
;;; -----------------------------------------------------------------------
(defmodule scmtoo
  (import (scheme scheme0 scmtoo0
           (only (main *redefine-imported-bindings*
                  *first-year-students*) eval)
           (only (*argv*
                  push-dynamic-variable
                  dynamic-variable-ref
                  pop-dynamic-variables) level1))
   export (*redefine-imported-bindings*
           push-dynamic-variable
           dynamic-variable-ref
           pop-dynamic-variables))
  (setq *first-year-students* t)
  (main *argv*)
)  ; end of module
