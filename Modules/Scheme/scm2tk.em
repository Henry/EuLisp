;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: scheme
;;;  Authors: Andreas Kind
;;; Description: Scheme interpreter/compiler entry point
;;;-----------------------------------------------------------------------------
(defmodule scm2tk
  (import (scheme tcltk scheme0 scmtoo0
           (only (main *redefine-imported-bindings*) eval)
           (only (*argv*
                  push-dynamic-variable
                  dynamic-variable-ref
                  pop-dynamic-variables
                  make
                  thread-reschedule
                  thread-start
                  <current-thread>) level1))
   export (*redefine-imported-bindings*
           push-dynamic-variable
           dynamic-variable-ref
           pop-dynamic-variables))

;;;-----------------------------------------------------------------------------
;;; Handle Tcl events on a thread; tk-main-loop would block
;;;-----------------------------------------------------------------------------
  (defun tcl-do-events ()
    ;; not define because it is compiled with youtoo, not scmtoo
    (if (= (tcl-do-one-event 1) 0)
        (thread-reschedule)
      ())
    (tcl-do-events))

  (thread-start (make <current-thread> function: tcl-do-events))

;;;-----------------------------------------------------------------------------
;;; Start-up the interpreter
;;;-----------------------------------------------------------------------------

  (main *argv*)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
