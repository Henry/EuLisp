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
;;; Title: Test text widget
;;;  Library: tcltk
;;;  Authors: J Garcia
;;;  Description:
;;    This exmaple show the way text widgets works, and some of
;;    the commands that can be done in YouToo/tk.
;;;-----------------------------------------------------------------------------

(defmodule tk_text
  (syntax (macros)
   import (level1
           tcltk))

;;;-----------------------------------------------------------------------------
;;; Callback Functions
;;;-----------------------------------------------------------------------------
(defun loadFile (entry text)
  (let (x
        (name (tk-get-value-widget entry)))
    (tk-delete text "1.0" "end")
    (tk-insert text "end" (fmt "File: ~s\n" name))
    (tk-text-tag-add text "big" "1.6" "1.0 lineend")
    (with-input-file
     (s name)
     (setq x (read s t (eos-default-value)))
     (while (null? (eq x (eos-default-value)))
       (tk-insert text "end" (fmt "~a" x))
       (setq x (read s () (eos-default-value)))))))

(defun change-configuration (text)
  (tk-conf-tag-text text "big" background: "Bisque3" borderwidth: "2"
                    font: "-Adobe-Helvetica-Medium-R-Normal--*-240-*"
                    relief: "raised"))
(defun go-in (text)
  (tk-conf-tag-text text "big" background: "SeaGreen2"))

(defun go-out (text)
  (tk-conf-tag-text text "big" background: "Bisque3"))

(defun test-text ()
  (let* ((frame-up (tk-make-frame ()))
         (text (tk-make-text frame-up relief: "raised" bd: 2))
         (scroll (tk-make-scrollbar frame-up))
         (frame-down (tk-make-frame ()))
         (file-to-read (tk-make-entry () width: 30))
         (read-button
          (tk-make-button () text: "Read File" command: loadFile
                          args: (list file-to-read text)))
         (label-info
          (tk-make-label () text: "File to read: "))
         (button
          (tk-make-button () text: "Change"
                          command: change-configuration
                          args: (list text))))

    (eul-associate text scroll 'vertical)

    (tk-bind-tag-text text "big" "<Enter>" go-in args: (list text))
    (tk-bind-tag-text text "big" "<Leave>" go-out args: (list text))
    (tk-pack frame-up frame-down side: "top")
    (tk-pack scroll side: "right" fill: "y")
    (tk-pack text side: "left")
    (tk-pack read-button label-info file-to-read button side: "left")
    )
  (Tk_MainLoop))

(test-text)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
