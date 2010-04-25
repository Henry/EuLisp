;;; ----------------------------------------------------------------------- ;;;
;;;  By J Garcia & University of Bath. All rights reserved.                 ;;;
;;; ----------------------------------------------------------------------- ;;;
;;; ---                         EuLisp System 'youtoo/tk'
;;; ----------------------------------------------------------------------- ;;;
;;;  Library: 
;;;  Authors: J Garcia
;;; Description: YouToo/Tk module to test buttons & bindings.
;;; ----------------------------------------------------------------------- ;;;
(defmodule tk_misc
  (syntax (macros)
   import (level1 tcl-tk)
   export (test-miscellaneous))
;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                      General variables to the module                     ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
(deflocal *x* 0)
(deflocal *y* 0)
(deflocal *z* 0)
(deflocal *t* 0)
;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                             Callback Functions                           ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
(defun sum ()
  (tk-destroy *y* *z*))
(defun buttonBind ()
  (print "I came into the button")
  (flush))
;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                              Test Function                               ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
(defun test-miscellaneous ()
  (tk-wm "title" () "Test Miscellaneous")
  (setq *x* (tk-make-button () fg: "red" text: "Hello" command: sum))
  (setq *y* (tk-make-label () text: "Bye Bye"))
  (setq *z* (tk-make-frame () width: "15m" height: "10m" relief: "raised" borderwidth: "4")) 
  (setq *t* (tk-make-button () fg: "blue" text: "Exit" command: tk-exit))
  (tk-pack *x* *y* *z* *t* padx: "2c" fill: "x" side: "bottom")
  (tk-bind *x* "<Enter>" buttonBind)
 
  (tk-conf-widget *x* relief: "raised" text: "Hello Again")
  (tk-conf-widget *y* text: "Goodbye")
 
  (Tk_MainLoop))
(test-miscellaneous)
)
 