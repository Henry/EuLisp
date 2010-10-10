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
;;; Title: Test image widget
;;;  Library: tcltk
;;;  Authors: J. Garcia
;;;  Maintainer: Henry G. Weller
;;;  Title:
;;    This exmaple show the way text widgets works, and some of
;;    the commands that can be done in YouToo/tk.
;;;-----------------------------------------------------------------------------

(defmodule tk_image
  (syntax (macros)
   import (level1
           tcltk))

;;;-----------------------------------------------------------------------------
;;; Callback Functions
;;;-----------------------------------------------------------------------------
(defun callback (but x)
  (tk-delete-image x)
  (tk-conf-widget but image: "" text: "No image now"))

(defun proving ()
  (let* ((imatge (tk-create-image 'photo file: "market.gif"))
         (button (tk-make-button () image: imatge command: callback
                                 args: (list self: imatge))))

    (tk-wm "title" () "proving")
    (tk-pack button)

    (format t "height: ~s\n" (tk-height-image imatge))
    (flush)
    (format t "width: ~s\n" (tk-width-image imatge))
    (flush)
    (format t "type: ~s\n" (tk-type-image imatge))
    (flush)
    (format t "names: ~s\n" (tk-names-image))
    (flush)
    (format t "types: ~s\n" (tk-types-image))
    (flush)

    (Tk_MainLoop)))

(proving)

;;;-----------------------------------------------------------------------------
)  ;; End of module tk_image
;;;-----------------------------------------------------------------------------
