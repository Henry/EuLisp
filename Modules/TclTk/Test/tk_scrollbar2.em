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
;;; Title: Test scrollbar & listbox widgets
;;;  Library: tcltk
;;;  Authors: J. Garcia
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule tk_scrollbar2
  (syntax (syntax-1)
   import (level-1
           tcltk))

(deflocal *lb1* ())
(deflocal *lb2* ())
(deflocal *lb3* ())
(deflocal *lb4* ())
(deflocal *scroll* ())
(deflocal i 0)

(defun Fun-scroll-callback (type entry . units)
  (tk-yview *lb1* type entry units)
  (tk-yview *lb2* type entry units)
  (tk-yview *lb3* type entry units)
  (tk-yview *lb4* type entry units))

(defun Fun-listbox-callback (first-entry last-entry)
  (tk-scroll-set *scroll* first-entry last-entry))

(defun prove ()
  (setq *scroll* (tk-make-scrollbar () command: Fun-scroll-callback))
  (setq *lb1* (tk-make-listbox () relief: "sunken"
                               yscrollcommand: Fun-listbox-callback))
  (setq *lb2* (tk-make-listbox () relief: "sunken"
                               yscrollcommand: Fun-listbox-callback))
  (setq *lb3* (tk-make-listbox () relief: "sunken"
                               yscrollcommand: Fun-listbox-callback))
  (setq *lb4* (tk-make-listbox () relief: "sunken"
                               yscrollcommand: Fun-listbox-callback))
  (tk-pack *lb1* *lb2* *lb3* *lb4* side: "left")
  (tk-pack *scroll* side: "right" fill: "y")
  (while (< i 20)
    (tk-insert *lb1* "end" i)
    (tk-insert *lb2* "end" i)
    (tk-insert *lb3* "end" i)
    (tk-insert *lb4* "end" i)
    (setq i (+ i 1)))
  (Tk_MainLoop))

(prove)

;;;-----------------------------------------------------------------------------
)  ;; End of module tk_scrollbar2
;;;-----------------------------------------------------------------------------
