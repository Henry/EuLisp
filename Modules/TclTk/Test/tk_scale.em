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
;;; Title: Test scale widgets
;;;  Library: tcltk
;;;  Authors: J. Garcia
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule tk_scale
  (syntax (macros)
   import (level1
           tcltk)
   export (test-scale))

;;;-----------------------------------------------------------------------------
;;; General variables to the module
;;;-----------------------------------------------------------------------------
(deflocal *red* 0)
(deflocal *green* 0)
(deflocal *blue* 0)
(deflocal *sample* 0)
(deflocal *exit-button* 0)

;;;-----------------------------------------------------------------------------
;;; Callback Functions
;;;-----------------------------------------------------------------------------
;; Notice that this callback functions receive one argument. That is because
;; scrollbar widget pass this argument to the callback function.
;; The next function converts the three integers R,G,B into a string in the form
;; #RRRRGGGGBBBB  , when each letter represents an hexadecimal number.
(defun newColour (value)
  (let* ((red-value (convert (tk-get-value-widget *red*) <fpi>))
         (green-value (convert (tk-get-value-widget *green*) <fpi>))
         (blue-value (convert (tk-get-value-widget *blue*) <fpi>))
         (aux ())
         new-colour)
    (do (lambda (c)
          (setq aux
                (cons (if (< c 16) (fmt "~x000" c)
                        (fmt "~x00" c)) aux)))
        (list blue-value green-value red-value))
    (setq new-colour (fmt "#~a~a~a" (car aux) (cadr aux) (caddr aux)))
    (tk-conf-widget *sample* background: new-colour)))

;;;-----------------------------------------------------------------------------
;;; General variables to the module
;;;-----------------------------------------------------------------------------
(defun test-scale ()
  (tk-wm "title" () "Test Scale")
  (setq *red*
        (tk-make-scale () command: newColour label: "Red" from: "0" to: "255"
                       length: "10c" orient: "horizontal"))

  (setq *green*
        (tk-make-scale () command: newColour label: "Green" from: "0" to: "255"
                       length: "10c" orient: "horizontal"))
  (setq *blue*
        (tk-make-scale () command: newColour label: "Blue" from: "0" to: "255"
                       length: "10c" orient: "horizontal"))
  (setq *sample*
        (tk-make-frame () borderwidth: "4" relief: "raised" height: "1.5c" width: "6c"))

  (setq *exit-button* (tk-make-button () text: "Exit" fg: "red" command: tk-exit))

  (tk-pack *red* *green* *blue* side: "top")
  (tk-pack *sample* pady: "2m" side: "bottom")
  (tk-pack *exit-button* side: "bottom" fill: "x" padx: "3m" pady: "3m")
  (Tk_MainLoop))

(test-scale)

;;;-----------------------------------------------------------------------------
)  ;; End of module tk_scale
;;;-----------------------------------------------------------------------------
