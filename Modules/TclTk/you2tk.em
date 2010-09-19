;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: tcltk
;;;  Authors: Andreas Kind
;;; Description: compiler/interpreter entry point with tck/tk
;;;-----------------------------------------------------------------------------
(defmodule you2tk
  (syntax (macros)
   import (level1 math eval tcltk macros)
   expose (tcltk))

;;;-----------------------------------------------------------------------------
;;; Handle Tcl events on a thread; tk-main-loop would block
;;;-----------------------------------------------------------------------------
(defun tcl-do-events ()
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
)  ;; End of module
;;;-----------------------------------------------------------------------------
