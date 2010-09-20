;;; Copyright 1997 J. Garcia & University of Bath
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
;;; Title: Test buttons & bindings.
;;;  Library: tcltk
;;;  Authors: J Garcia
;;;-----------------------------------------------------------------------------

(defmodule tk_misc
  (syntax (macros)
   import (level1
           tcltk)
   export (test-miscellaneous))

;;;-----------------------------------------------------------------------------
;;; General variables to the module
;;;-----------------------------------------------------------------------------
(deflocal *x* 0)
(deflocal *y* 0)
(deflocal *z* 0)
(deflocal *t* 0)
(deflocal *l* 0)

;;;-----------------------------------------------------------------------------
;;; Callback Functions
;;;-----------------------------------------------------------------------------
(defun sum ()
  (tk-destroy *y* *z*))

(defun buttonBind ()
  (print "I came into the button")
  (flush))

;;;-----------------------------------------------------------------------------
;;; Test Function
;;;-----------------------------------------------------------------------------
(defun test-miscellaneous ()
  (tk-wm "title" () "Test Miscellaneous")
  (setq *x* (tk-make-button () fg: "red" text: "Hello" command: sum))
  (setq *y* (tk-make-label () text: "Bye Bye"))
  (setq *z* (tk-make-frame () width: "15m" height: "10m" relief: "raised" borderwidth: "4"))
  (setq *l* (tk-make-label *z* text: "A Frame"))
  (tk-pack *l* side: "top" fill: "x")
  (setq *t* (tk-make-button () fg: "blue" text: "Exit" command: tk-exit))
  (tk-pack *x* *y* *z* *t* padx: "2c" fill: "x" side: "bottom")
  (tk-bind *x* "<Enter>" buttonBind)

  (tk-conf-widget *x* relief: "raised" text: "Hello Again")
  (tk-conf-widget *y* text: "Goodbye")

  (Tk_MainLoop))

(test-miscellaneous)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
