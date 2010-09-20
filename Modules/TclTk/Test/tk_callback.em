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
;;; Title: Test callbacks
;;;  Library: tcltk
;;;  Authors: J Garcia
;;;-----------------------------------------------------------------------------

(defmodule tk_callback
  (syntax (macros)
   import (level1
           tcltk))

;;; Callback Functions
(defclass <node> ()
  ((name accessor: name-node
         keyword: name:))
  predicate: node-p)

(defun callback (button node)
  (format t "I am executing the callback function\n")
  (format t "button: ~s\n" button)
  ;; The next line shows how to use the flash command of the buttons
  (tk-button-flash button)
  (format t "node: ~s\n" (name-node node))
  (flush))

(defun test_callback ()
  (let* ((node1 (make <node> name: 'node1))
         (node2 (make <node> name: 'node2))
         (but (tk-make-button () text: "hello-button1" command: callback args: (list self: node1)))
         (but2 (tk-make-button () text: "hello-button2" command: callback args: (list self: node2)))
         (but-exit (tk-make-button () text: "Exit" fg: "red" command: tk-exit)))
    (tk-pack but but2 but-exit padx: "10" pady: "3"))

  (Tk_MainLoop))
(test_callback)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
