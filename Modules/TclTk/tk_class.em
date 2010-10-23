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
;;; Title: Basic operations for the widget classes
;;;  Library: tcltk
;;;  Authors: J. Garcia
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule tk_class
  (syntax (macros)
   import (level1
           tk_general)
   export (tk-make-button
           tk-make-label
           tk-make-frame
           tk-make-canvas
           tk-make-checkbutton
           tk-make-entry
           tk-make-listbox
           tk-make-menu
           tk-make-menubutton
           tk-make-message
           tk-make-radiobutton
           tk-make-scale
           tk-make-scrollbar
           tk-make-text
           tk-make-toplevel
           tk-conf-widget
           tk-menu-add
           tk-insert
           tk-delete
           tk-xview tk-yview
           tk-get-value-widget
           tk-scroll-set
           tk-listbox-curselection
           tk-button-flash
           eul-associate
           tk-set-value-widget))

;;;-----------------------------------------------------------------------------
;;; Extern Declarations (Call foreign functions.)
;;;-----------------------------------------------------------------------------
(defextern eul_tk_create_widget (<string> <string> ptr) ptr)
(defextern eul_tk_conf_widget (<string> ptr ptr) ptr)
(defextern eul_tk_get_value_widget (<string> ptr ptr) ptr)
(defextern eul_tk_set_value_widget (<string> ptr <string>) ptr)
(defextern eul_tk_button_flash (<string> ptr) ptr)
(defextern eul_add_menu_command (<string> ptr ptr) ptr)
(defextern eul_insert_command (<string> ptr <string> <string>) ptr)
(defextern eul_delete_command (<string> ptr <string> ptr) ptr)
(defextern eul_view_cmd (<string> ptr <string> <string> ptr <string>) ptr)
;(defextern eul_yview_command (<string> ptr <string> <string> ptr) ptr)
(defextern eul_tk_listbox_cmd (<string> ptr <string>) ptr)
(defextern eul_set_scrollbar_command (<string> ptr <string> <string>) ptr)

;;;-----------------------------------------------------------------------------
;;; Generic Functions definitions
;;;-----------------------------------------------------------------------------
(defgeneric tk-conf-widget ((x <tk-object>) . options))
(defgeneric tk-get-value-widget ((x <tk-object>) . indexs))
(defgeneric tk-set-value-widget ((x <tk-object>) index))
(defgeneric tk-xview ((object <tk-object>)
                      (type <string>) (entry <string>) units))
(defgeneric tk-yview ((object <tk-object>)
                      (type <string>) (entry <string>) units))
(defgeneric tk-insert ((object <tk-object>) (type <string>) text))
(defgeneric tk-delete ((object <tk-object>) (first <string>) . last))

;;;-----------------------------------------------------------------------------
;;; tk-create-widget
;;;-----------------------------------------------------------------------------
(defun tk-create-widget (type parent options)
  (if (and parent (null? (tk-object? parent)))
      (progn
        (format "Can't create ~s. Parent incorrect.\n" type)
        (flush)
        ())
    (let (x)
      (cond
        ((eq type 'button)
         (setq x (make <tk-button>)))
        ((eq type 'label)
         (setq x (make <tk-label>)))
        ((eq type 'frame)
         (setq x (make <tk-frame>)))
        ((eq type 'canvas)
         (setq x (make <tk-canvas>)))
        ((eq type 'checkbutton)
         (setq x (make <tk-checkbutton>)))
        ((eq type 'entry)
         (setq x (make <tk-entry>)))
        ((eq type 'listbox)
         (setq x (make <tk-listbox>)))
        ((eq type 'menu)
         (setq x (make <tk-menu>)))
        ((eq type 'menubutton)
         (setq x (make <tk-menubutton>)))
        ((eq type 'message)
         (setq x (make <tk-message>)))
        ((eq type 'radiobutton)
         (setq x (make <tk-radiobutton>)))
        ((eq type 'scale)
         (setq x (make <tk-scale>)))
        ((eq type 'scrollbar)
         (setq x (make <tk-scrollbar>)))
        ((eq type 'text)
         (setq x (make <tk-text>)))
        ((eq type 'toplevel)
         (setq x (make <tk-toplevel>))))
      (and (tk-object? parent)
           (put-on x parent))
      ((setter tk-handler) x
       (eul_tk_create_widget
        (symbol-name type) (tk-name x) (as-c-options options x)))
      x)))

;;;-----------------------------------------------------------------------------
;;; Functions for Button
;;;-----------------------------------------------------------------------------
()
(defun tk-make-button (parent . options)
  (tk-create-widget 'button parent options))

(defun tk-button-flash (but)
  (and (tk-button? but)
       (eul_tk_button_flash (tk-name but) (tk-handler but))))

;;;-----------------------------------------------------------------------------
;;; Functions for Label
;;;-----------------------------------------------------------------------------
(defun tk-make-label (parent . options)
  (tk-create-widget 'label parent options))

;;;-----------------------------------------------------------------------------
;;; Functions for Frames
;;;-----------------------------------------------------------------------------
(defun tk-make-frame (parent . options)
  (tk-create-widget 'frame parent options))

;;;-----------------------------------------------------------------------------
;;; Functions for Canvas
;;;-----------------------------------------------------------------------------
(defun tk-make-canvas (parent . options)
  (tk-create-widget 'canvas parent options))

;;;-----------------------------------------------------------------------------
;;; Functions for CheckButton
;;;-----------------------------------------------------------------------------
(defun tk-make-checkbutton (parent . options)
  (tk-create-widget 'checkbutton parent options))

;;;-----------------------------------------------------------------------------
;;; Functions for Entry
;;;-----------------------------------------------------------------------------
(defun tk-make-entry (parent . options)
  (tk-create-widget 'entry parent options))

(defmethod tk-insert ((entry <tk-entry>) (type <string>) (text <string>))
  (eul_insert_command (tk-name entry) (tk-handler entry) type text))

(defmethod tk-delete ((entry <tk-entry>) (first <string>) . last)
  (and (or (and (binary= (list-size last) 1)
                (string? (car last)))
           (null? last))
       (eul_delete_command (tk-name entry) (tk-handler entry) first last)))

(defmethod tk-xview ((entryWidget <tk-entry>)
                     (type <string>)
                     (entry <string>)
                     units)
  (eul_view_cmd (tk-name entryWidget)
                (tk-handler entryWidget)
                type entry units "xview"))

(defmethod tk-get-value-widget ((entry <tk-entry>) . indexs)
  (and (null? indexs)
       (eul_tk_get_value_widget (tk-name entry) (tk-handler entry) indexs)))

;;;-----------------------------------------------------------------------------
;;; Functions for ListBox
;;;-----------------------------------------------------------------------------
(defun tk-make-listbox (parent . options)
  (tk-create-widget 'listbox parent options))

(defmethod tk-insert ((listbox <tk-listbox>) (type <string>) (text <string>))
  (eul_insert_command (tk-name listbox) (tk-handler listbox) type text))

(defmethod tk-insert ((listbox <tk-listbox>) (type <string>) (num  <fpi>))
  (eul_insert_command
   (tk-name listbox) (tk-handler listbox) type (convert num <string>)))

(defmethod tk-delete ((listbox <tk-listbox>) (first <string>) . last)
  (and (or (and (binary= (list-size last) 1)
                (string? (car last)))
           (null? last))
       (eul_delete_command
        (tk-name listbox) (tk-handler listbox) first last)))

(defmethod tk-xview ((listbox <tk-listbox>) (type <string>) (entry <string>)
                     units)
  (eul_view_cmd (tk-name listbox) (tk-handler listbox) type entry units
                "xview"))

(defmethod tk-yview ((listbox <tk-listbox>) (type <string>) (entry <string>)
                     units)
  (eul_view_cmd (tk-name listbox) (tk-handler listbox) type entry units
                "yview"))

(defun tk-listbox-curselection (listbox)
  (and (tk-listbox? listbox)
       (eul_tk_listbox_cmd (tk-name listbox) (tk-handler listbox)
                           "curselection")))

;;;-----------------------------------------------------------------------------
;;; Functions for Menu
;;;-----------------------------------------------------------------------------
(defun tk-make-menu (parent . options)
  (tk-create-widget 'menu parent options))

(defgeneric tk-menu-add ((menu <tk-menu>) (type <symbol>) . options))

(defmethod tk-menu-add ((menu <tk-menu>) (type <symbol>) . options)
  (let ((newoptions (cons (symbol-name type) options)))
    (eul_add_menu_command (tk-name menu) (tk-handler menu)
                          (as-c-options newoptions menu))))

;;;-----------------------------------------------------------------------------
;;; Functions for MenuButton
;;;-----------------------------------------------------------------------------
(defun tk-make-menubutton (parent . options)
  (tk-create-widget 'menubutton parent options))

;;;-----------------------------------------------------------------------------
;;; Functions for Message
;;;-----------------------------------------------------------------------------
(defun tk-make-message (parent . options)
  (tk-create-widget 'message parent options))

;;;-----------------------------------------------------------------------------
;;; Functions for RadioButton
;;;-----------------------------------------------------------------------------
(defun tk-make-radiobutton (parent . options)
  (tk-create-widget 'radiobutton parent options))

;;;-----------------------------------------------------------------------------
;;; Functions for Scale
;;;-----------------------------------------------------------------------------
(defun tk-make-scale (parent . options)
  (tk-create-widget 'scale parent options))

(defmethod tk-get-value-widget ((x <tk-scale>) . indexs)
  (eul_tk_get_value_widget (tk-name x) (tk-handler x) indexs))

(defmethod tk-set-value-widget ((x <tk-scale>) index)
  (let ((index-str (convert index <string>)))
    (eul_tk_set_value_widget (tk-name x) (tk-handler x) index-str)))

;;;-----------------------------------------------------------------------------
;;; Functions for Scrollbar
;;;-----------------------------------------------------------------------------
(defun tk-make-scrollbar (parent . options)
  (tk-create-widget 'scrollbar parent options))

(defgeneric tk-scroll-set ((scroll <tk-scrollbar>) (first-entry <string>)
                           (last-entry <string>)))

(defmethod tk-scroll-set ((scroll <tk-scrollbar>) (first-entry <string>)
                          (last-entry <string>))
  (eul_set_scrollbar_command (tk-name scroll) (tk-handler scroll)
                             first-entry last-entry))

;;;-----------------------------------------------------------------------------
;;; Functions for Text
;;;-----------------------------------------------------------------------------
(defun tk-make-text (parent . options)
  (tk-create-widget 'text parent options))

(defmethod tk-insert ((text-widget <tk-text>) (type <string>) (text <string>))
  (eul_insert_command
   (tk-name text-widget) (tk-handler text-widget) type text))

(defmethod tk-delete ((text <tk-text>) (first <string>) . last)
  (and (or (and (binary= (list-size last) 1)
                (string? (car last)))
           (null? last))
       (eul_delete_command (tk-name text) (tk-handler text) first last)))

(defmethod tk-xview ((text <tk-text>) (type <string>) (entry <string>) units)
  (eul_view_cmd (tk-name text) (tk-handler text) type entry units "xview"))

(defmethod tk-yview ((text <tk-text>) (type <string>) (entry <string>) units)
  (eul_view_cmd (tk-name text) (tk-handler text) type entry units "yview"))

(defmethod tk-get-value-widget ((x <tk-text>) . indexs)
  ;; Due to we are calling a C foreign function we need to say how many
  ;; arguments there are in this list. The way this foreign function works
  ;; needs to have the quantity of elements in the last argument.
  (let ((size (list-size indexs)))
    (and  (or (binary= size 1)
              (binary= size 2))
          (eul_tk_get_value_widget (tk-name x) (tk-handler x)
                                   (cons size indexs)))))

;;;-----------------------------------------------------------------------------
;;; Functions for Top-level
;;;-----------------------------------------------------------------------------
(defun tk-make-toplevel (parent . options)
  (tk-create-widget 'toplevel parent options))

;;;-----------------------------------------------------------------------------
;;; Global functions for every widget
;;;-----------------------------------------------------------------------------
(defun put-on (x y)
  (let* ((x-name (tk-name x))
         (y-name (tk-name y)))
    ((setter tk-name) x (string-append y-name x-name))
    x))

(defmethod tk-conf-widget ((x <tk-object>) . options)
  (eul_tk_conf_widget (tk-name x) (tk-handler x) (as-c-options options x)))

(defmethod generic-write ((x <tk-object>) (s <stream>))
  (sformat s "<~a: ~a>" (class-name (class-of x)) (tk-name x)))

;;;-----------------------------------------------------------------------------
;;; eul-associate Function
;;;-----------------------------------------------------------------------------
;;  This widget associates one widget with one scrollbar. Only canvas, entry,
;; listbox and text are allow to have scrollbars. The user also have to give
;; the orientation of the scrollbar.
(defun eul-associate (widget scroll type)
  (if (and (or (tk-canvas? widget)
               (tk-entry? widget)
               (tk-listbox? widget)
               (tk-text? widget))
           (tk-scrollbar? scroll)
           (or (eq type 'horizontal)
               (eq type 'vertical)))
      (let (str1
            (str2 (fmt "~a set" (tk-name scroll))))
        (setq str1
              (if (eq type 'horizontal)
                  (fmt "~a xview" (tk-name widget))
                (fmt "~a yview" (tk-name widget))))
        (tk-conf-widget scroll command: str1)
        (cond ((eq type 'horizontal)
               (tk-conf-widget widget xscrollcommand: str2))
              ((and (eq type 'vertical)
                    (null? (tk-entry? widget)))
               (tk-conf-widget widget yscrollcommand: str2))
              (t
               (format "entry widget does not allow vertical scrollbar\n")
               (flush))))
    (progn
      (format "eul-associate has received invalid parameters\n")
      (flush))))

;;;-----------------------------------------------------------------------------
)  ;; End of module tk_class
;;;-----------------------------------------------------------------------------
