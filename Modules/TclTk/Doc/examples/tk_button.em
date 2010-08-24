;;; ----------------------------------------------------------------------- ;;;
;;;  By J Garcia & University of Bath. All rights reserved.                 ;;;
;;; ----------------------------------------------------------------------- ;;;
;;; ---                         EuLisp System 'youtoo/tk'
;;; ----------------------------------------------------------------------- ;;;
;;;  Library:
;;;  Authors: J Garcia
;;; Description: YouToo/Tk module to test buttons widgets.
;;; ----------------------------------------------------------------------- ;;;
(defmodule tk_button
  (syntax (macros)
   import (level1 tcl-tk)
   export (test-buttons))
;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                          Global variables to the Module                  ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
(deflocal *result1* 0)
(deflocal *result2* 0)
;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                             Callback Functions                           ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
(defun Value-frame1 ()
  (let ((auxtext (format () "Value: ~a" (tk-get-variable "pts"))))
    (tk-conf-widget *result1* text: auxtext)))
(defun Values-frame2 ()
  (let* ((bold (tk-get-variable "bold"))
         (italic (tk-get-variable "italic"))
         (underline (tk-get-variable "underline"))
         (auxtext (format () "Values: ~a ~a ~a" bold italic underline)))
    (tk-conf-widget *result2* text: auxtext)))
;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                            Main Test Function                            ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;

(defun test-buttons ()
  (tk-wm "title" () "Test Buttons")
  (let ((left (tk-make-frame ()))
        (right (tk-make-frame ()))
        (pts8
         (tk-make-radiobutton () text: "8 points" relief: "flat" variable: "pts" value: 8))
        (pts10
         (tk-make-radiobutton () text: "10 points" relief: "flat" variable: "pts" value: 10))
        (pts12
         (tk-make-radiobutton () text: "12 points" relief: "flat" variable: "pts" value: 12))
        (pts18
         (tk-make-radiobutton ()text: "18 points" relief: "flat" variable: "pts" value: 18))
        (pts24
         (tk-make-radiobutton () text: "24 points" relief: "flat" variable: "pts" value: 24))
        (bold (tk-make-checkbutton () text: "Bold" relief: "flat" variable: "bold"))
        (italic (tk-make-checkbutton () text: "Italic" relief: "flat" variable: "italic"))
        (underline
         (tk-make-checkbutton () text: "Underline" relief: "flat" variable: "underline"))
        (button1
         (tk-make-button () text: "Value" fg: "red" command: Value-frame1))
        (button2 (tk-make-button () text: "Values" fg: "red" command: Values-frame2))
        (exit (tk-make-button () text: "Exit" fg: "Blue" command: tk-exit)))

    (tk-pack left side: "left" padx: "1c" pady: "1c")
    (tk-pack right side: "right" padx: "1c" pady: "1c")
    (setq *result1* (tk-make-label () text: "Value:" fg: "Brown"))
    (setq *result2* (tk-make-label () text: "Values:" fg: "Brown"))

    (tk-pack pts8 pts10 pts12 pts18 pts24 in: left side: "top" anchor: "w")
    (tk-pack button1 in: left pady: "5m")
    (tk-pack *result1* in: left anchor: "w" pady: "5m")

    (tk-pack bold italic underline in: right side: "top" anchor: "w")
    (tk-pack button2 in: right pady: "1c")
    (tk-pack *result2* in: right anchor: "w" pady: "5m")
    (tk-pack exit side: "bottom" pady: "3m")

    )
  (Tk_MainLoop))
(test-buttons))
