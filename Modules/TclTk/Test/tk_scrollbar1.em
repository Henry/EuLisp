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
;;; Title: Test scrollbar & listbox widgets.
;;;  Library: tcltk
;;;  Authors: J. Garcia
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule tk_scrollbar1
  (syntax (macros)
   import (level-1
           tcltk)
   export (test-scrollbar))

;;;-----------------------------------------------------------------------------
;;; Callback Functions
;;;-----------------------------------------------------------------------------
(defun take-value ()
  (let (result)
    (setq result (tk-selection-get "LINE"))
    (format t "result: ~a" result)))

;;;-----------------------------------------------------------------------------
;;; Test Function
;;;-----------------------------------------------------------------------------
(defun test-scrollbar ()
  (tk-wm "title" () "Test Scrollbar")
  (let ((i 0)
        (frame-left (tk-make-frame () width: "3c" height: "4c"))
        (frame-right (tk-make-frame () width: "6c" height: "4c"))
        (numbers-lb (tk-make-listbox ()))
        (text (tk-make-text () relief: "sunken" bd: "2"))
        (scroll-1 (tk-make-scrollbar ()))
        (scroll-2 (tk-make-scrollbar ()))
        (exit-but (tk-make-button () text: "Exit" command: tk-exit))
        str)

    (eul-associate text scroll-2 'vertical)
    (eul-associate numbers-lb scroll-1 'vertical)

    (while (< i (* 14 4))
      (setq str (convert i <string>))
      (tk-insert numbers-lb "end" str)
      (setq i (+ i 2)))


    (tk-bind numbers-lb "<Button-1>" take-value)
    (tk-insert text "end" (fmt "Line 1.\n2\n3\n4\n5\n...\n"))


    (tk-pack frame-left side: "left")
    (tk-pack frame-right side: "right")
    (tk-pack numbers-lb in: frame-left side: "left")
    (tk-pack scroll-1 in: frame-left side: "right" fill: "y")
    (tk-pack text in: frame-right side: "left")
    (tk-pack scroll-2 in: frame-right side: "right" fill: "y")
    (tk-pack exit-but side: "bottom" pady: "5")
    )

  (Tk_MainLoop))

(test-scrollbar)

;;;-----------------------------------------------------------------------------
)  ;; End of module tk_scrollbar1
;;;-----------------------------------------------------------------------------
