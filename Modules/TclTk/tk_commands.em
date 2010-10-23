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
;;; Title: General functions
;;;  Library: tcltk
;;;  Authors: J. Garcia
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule tk_commands
  (syntax (macros)
   import (level1
           tk_general)
   export (tk-pack
           tk-bind
           tk-destroy
           tk-exit
           tk-get-variable
           tk-set-variable
           tk-wm
           tk-selection-get
           tk-get-result
           tk-focus
           tk-map-widget
           tk-unmap-widget
           tk-grab-set
           tk-bell))

(defextern eul_tk_pack (<string> ptr) ptr)
(defextern eul_tk_bind (<string> <string> <string> ptr) ptr)
(defextern eul_tk_destroy (<string> ptr) ptr)
(defextern eul_tk_wm (<string> ptr) ptr)
(defextern eul_tk_selection_get (ptr) ptr)
(defextern eul_tk_get_result () ptr)
(defextern eul_tk_focus (<string> <string>) ptr)
(defextern eul_tk_map_widget (<string>) ptr)
(defextern eul_tk_unmap_widget (<string>) ptr)
(defextern eul_tk_bell () ptr)

(defun tk-bell () (eul_tk_bell))

(defextern eul_tk_grab_set (<string>) ptr)

;; the next function only allows getting int variables.
(defextern eul_tk_get_variable (<string>) <fpi>)

;; To store an int variable you need to pass as a string.
(defextern eul_tk_set_variable (<string> <string>) ptr)

;;;-----------------------------------------------------------------------------
;;; tk-pack Function
;;;-----------------------------------------------------------------------------
;; This functions allows the user to call the Tcl Function pack.
(defun tk-pack (x . options)
  (and (tk-object? x)
       (eul_tk_pack (tk-name x) (as-c-options options))))

;;;-----------------------------------------------------------------------------
;;; tk-bind Method
;;;-----------------------------------------------------------------------------
;; This method allows the users to bind object with events. In case that the
;; the event occurs, the function -function- will be called.
(defgeneric tk-bind ((x <tk-object>) event function . args))

;; as-c-accessors is giving back a list of lists.
;; The first list in the list has to be passed to the foreign function
;; eul_tk_bind.
;; The second list of the list has to be given to the tk_allocate_registers, in order
;; to be able to give back to the callback more parameters.
(defmethod tk-bind ((x <tk-object>) event function . args)
  (let ((args2 (as-c-accessors args))
        (function-key (symbol-name (gensym (symbol-name (function-name function))))))
    (tk_allocate_registers function-key (cons function (cadr args2)))
    (eul_tk_bind (tk-name x) event function-key (car args2))))

;;;-----------------------------------------------------------------------------
;;; tk-destroy Method
;;;-----------------------------------------------------------------------------
;; This method allows the user to destroy a several number of widgets. This
;; method requires at least one widget to be destroyed.
(defgeneric tk-destroy ((x <tk-object>) . more-objects))
(defmethod tk-destroy ((x <tk-object>) . more-objects)
  (eul_tk_destroy (tk-name x) (as-c-options more-objects)))

;;;-----------------------------------------------------------------------------
;;; tk-exit Function
;;;-----------------------------------------------------------------------------
;; This function permits the user to finish the interaction with YouToo system.
;; Instead of calling the exit command of Tcl, we prefer to destroy the root
;; window to achieve the same effect.
(defun tk-exit ()
  (eul_tk_destroy "." (as-c-options '())))

;;;-----------------------------------------------------------------------------
;;; tk-get-variable Function
;;;-----------------------------------------------------------------------------
;; This function enables the user to consult the value of a Tcl variable.
(defun tk-get-variable (name-var)
  (let (result)
    (setq result (eul_tk_get_variable name-var))
    result))

;;;-----------------------------------------------------------------------------
;;; tk-set-variable Function
;;;-----------------------------------------------------------------------------
;; This function allows the user to initialize or modify a variable in the
;; Tcl world.
(defun tk-set-variable (name-var value)
  (eul_tk_set_variable name-var value))

;;;-----------------------------------------------------------------------------
;;; tk-wm Function
;;;-----------------------------------------------------------------------------
;; This method permits to call the widow manager. The user will be able to
;; arrange some aspects of the general window.
(defgeneric tk-wm ((operation <string>) widget . options))
(defmethod tk-wm ((operation <string>) (widget <tk-object>) . options)
  (let* ((auxlist (as-c-options options))
         (elements (car auxlist)))
    (eul_tk_wm operation (cons (+ elements 1) (cons (tk-name widget) (cdr auxlist))))))
(defmethod tk-wm ((operation <string>) (widget <null>) . options)
  (let* ((auxlist (as-c-options options))
         (elements (car auxlist)))
    (eul_tk_wm operation (cons (+ elements 1) (cons (fmt ".") (cdr auxlist))))))

;;;-----------------------------------------------------------------------------
;;; tk-get-result Function
;;;-----------------------------------------------------------------------------
;; This function allows the user to consult the string error.
;; If there is no error, the string will be empty.
(defun tk-get-result ()
  (eul_tk_get_result))

;;;-----------------------------------------------------------------------------
;;; tk-selection-get Function
;;;-----------------------------------------------------------------------------
(defun tk-selection-get option
  (if (null? option)
      (eul_tk_selection_get '())
    (eul_tk_selection_get option)))

;;;-----------------------------------------------------------------------------
;;; tk-focus Function
;;;-----------------------------------------------------------------------------
(defun tk-focus args
  (cond
    ((null? args)
     (eul_tk_focus "nameWindow" ""))
    ((and (car args) (null? (cdr args)))
     (cond
       ((null? (car args))
        (eul_tk_focus "." ""))
       ((tk-item-canvas? (car args))
        (eul_tk_focus (tk-name (car args)) (tk-item-canvas-id (car args))))
       ((tk-object? (car args))
        (eul_tk_focus (tk-name (car args)) ""))
       (t
        ())))))

;;;-----------------------------------------------------------------------------
;;; tk-map-widget Function
;;;-----------------------------------------------------------------------------
(defun tk-map-widget (widget)
  (cond ((null? widget)
         (eul_tk_map_widget "."))
        ((tk-object? widget)
         (eul_tk_map_widget (tk-name widget)))
        (t
         ())))

;;;-----------------------------------------------------------------------------
;;; tk-unmap-widge  Function
;;;-----------------------------------------------------------------------------
(defun tk-unmap-widget (widget)
  (cond ((null? widget)
         (eul_tk_unmap_widget "."))
        ((tk-object? widget)
         (eul_tk_unmap_widget (tk-name widget)))
        (t
         ())))

;;;-----------------------------------------------------------------------------
;;; tk-grab-set  Function
;;;-----------------------------------------------------------------------------
;; This function is only permitted if the received widget is a top-level widget.
;; Only local grab will be set. No global grabs are possible.
(defun tk-grab-set (toplevel)
  (cond ((null? (tk-toplevel? toplevel))
         ())
        (t
         (eul_tk_grab_set (tk-name toplevel)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module tk_commands
;;;-----------------------------------------------------------------------------
