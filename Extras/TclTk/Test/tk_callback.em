;;; ----------------------------------------------------------------------- ;;;
;;;  By Julio Garcia Moreno & University of Bath.                           ;;;
;;; ----------------------------------------------------------------------- ;;;
;;;                     EuLisp System 'youtoo/tk'
;;; ----------------------------------------------------------------------- ;;;
;;;  Library: tcltk
;;;  Authors: Julio Garcia Moreno
;;;  Description:  This module show the use of callbacks in YouToo/Tcl-tk
;;;             
;;; ----------------------------------------------------------------------- ;;;
(defmodule tk_callback
  (syntax (macros)
   import (level1 tcltk)
   )
;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                             Callback Functions                           ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
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
)
