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
;;; Title: Test menus
;;;  Library: tcltk
;;;  Authors: J Garcia
;;;-----------------------------------------------------------------------------

(defmodule tk_menu
  (syntax (macros)
   import (level1
           tcltk)
   export (test-menu))

;;;-----------------------------------------------------------------------------
;;; Global variables to the Module
;;;-----------------------------------------------------------------------------
;; The next variable will be used in the createSquare & deleteSquare functions.
(deflocal *auxiliar* ())

;;;-----------------------------------------------------------------------------
;;; Commands called by the menu
;;;-----------------------------------------------------------------------------
(defun createSquare ()
  (and (null? *auxiliar*)
       (setq *auxiliar*
             (tk-make-frame ()
                            height: "15m" width: "15m"
                            relief: "sunken" borderwidth: "5"))
       (tk-pack *auxiliar* side: "bottom")))

(defun deleteSquare ()
  (tk-destroy *auxiliar*)
  (setq *auxiliar* ()))

(defun createText ()
  (print "createText doesn't actually do anything yet."))

(defun noViews ()
  (print "No Views doesn't actually do anything."))
(defun noEdit ()
  (print "No Edit doesn't actually do anything."))

;;;-----------------------------------------------------------------------------
;;; Main Function test
;;;-----------------------------------------------------------------------------
(defun test-menu ()
  (tk-wm "title" () "Test Menus")
  (let* ((mbar (tk-make-frame () relief: "raised" bd: "2"))
         (dummy (tk-make-frame () width: "10c" height: "5c"))
         (file (tk-make-menubutton mbar))
         (edit (tk-make-menubutton mbar))
         (graphics (tk-make-menubutton mbar))
         (text (tk-make-menubutton mbar))
         (view (tk-make-menubutton mbar))
         (help (tk-make-menubutton mbar))
         (menu-file (tk-make-menu file tearoff: "0"))
         (menu-edit (tk-make-menu edit tearoff: "0"))
         (menu-graphics (tk-make-menu graphics tearoff: "0"))
         (menu-text (tk-make-menu text tearoff: "0"))
         (menu-view (tk-make-menu view tearoff: "0"))
         (menu-help (tk-make-menu help tearoff: "0"))
         (menu-graphics-colour (tk-make-menu menu-graphics tearoff: "0"))
         (menu-graphics-width (tk-make-menu menu-graphics tearoff: "0")))
    (tk-pack mbar dummy side: "top" fill: "x")

    ;; Because is necessary that the menus were descendents of the menubutton
    ;; we have to put on the widgets.

    (tk-conf-widget file text: "File" underline: "0" menu: menu-file)
    (tk-conf-widget edit text: "Edit" underline: "0" menu: menu-edit)
    (tk-conf-widget graphics text: "Graphics" underline: "0"
                    menu: menu-graphics)
    (tk-conf-widget text text: "Text" underline: "0" menu: menu-text)
    (tk-conf-widget view text: "View" underline: "0" menu: menu-view)
    (tk-conf-widget help text: "Help" underline: "0" menu: menu-help)
    ;; Creation of the File menu.
    (tk-menu-add menu-file 'command label: "Exit" command: tk-exit)
    ;; Creation of the Edit menu.
    (tk-menu-add menu-edit 'command label: "No Edit" command: noEdit)

    ;; Creation of the Text menu.
    (tk-menu-add menu-text 'checkbutton label: "Bold" variable: "bold")
    (tk-menu-add menu-text 'checkbutton label: "Italic" variable: "italic")
    (tk-menu-add menu-text 'checkbutton label: "Underline"
                 variable: "underline")
    (tk-menu-add menu-text 'separator)
    (tk-menu-add menu-text 'radiobutton label: "Times"
                 variable: "font" value: "times")
    (tk-menu-add menu-text 'radiobutton label: "Helvetica"
                 variable: "font" value: "helvetica")
    (tk-menu-add menu-text 'radiobutton label: "Courier"
                 variable: "font" value: "courier")

    (tk-menu-add menu-text 'separator)
    (tk-menu-add menu-text 'command label: "Create Square"
                 command: createSquare)
    (tk-menu-add menu-text 'command label: "Delete Square"
                 command: deleteSquare)
    (tk-menu-add menu-text 'command label: "Create Text"
                 command: createText)
    ;; Creation of the View menu.
    (tk-menu-add menu-view 'command label: "No Views" command: noViews)
    ;; Creation of the Graphics menu

    (tk-menu-add menu-graphics 'cascade label: "letter width"
                 menu: menu-graphics-width)
    (tk-menu-add menu-graphics 'cascade label: "colour"
                 menu: menu-graphics-colour)

    (tk-menu-add menu-graphics-width 'radiobutton label: "0.25 point"
                 variable: "linewidth" value: "0.25")
    (tk-menu-add menu-graphics-width 'radiobutton label: "0.50 point"
                 variable: "linewidth" value: "0.50")
    (tk-menu-add menu-graphics-width 'radiobutton label: "1 point"
                 variable: "linewidth" value: "1")
    (tk-menu-add menu-graphics-width 'radiobutton label: "2 point"
                 variable: "linewidth" value: "2")

    (tk-menu-add menu-graphics-colour 'radiobutton label: "Red"
                 variable: "colour" value: "red")
    (tk-menu-add menu-graphics-colour 'radiobutton label: "Blue"
                 variable: "colour" value: "blue")
    (tk-menu-add menu-graphics-colour 'radiobutton label: "Green"
                 variable: "colour" value: "green")
    (tk-menu-add menu-graphics-colour 'radiobutton label: "Black"
                 variable: "colour" value: "black")
    (tk-pack file edit graphics text view side: "left")
    (tk-pack help side: "right"))
  (Tk_MainLoop))

(test-menu)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
