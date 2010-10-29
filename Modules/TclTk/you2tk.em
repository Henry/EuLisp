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
;;; Title: compiler/interpreter entry point with tck/tk
;;;  Library: tcltk
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule you2tk
  (syntax (macros)
   import (level-1
           math
           eval
           tcltk
           macros)
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
)  ;; End of module you2tk
;;;-----------------------------------------------------------------------------
