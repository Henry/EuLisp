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
)  ;; End of module
;;;-----------------------------------------------------------------------------
