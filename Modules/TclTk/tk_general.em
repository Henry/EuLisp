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

(defmodule tk_general
  (syntax (syntax-1)
   import (level-1)
   export (<tk-button>
           <tk-label>
           <tk-frame>
           <tk-canvas>
           <tk-checkbutton>
           <tk-entry>
           <tk-listbox>
           <tk-menu>
           <tk-menubutton>
           <tk-message>
           <tk-radiobutton>
           <tk-scale>
           <tk-scrollbar>
           <tk-text>
           <tk-toplevel>
           <tk-object>
           <tk-item-canvas>
           eul_initialize_tk
           tk-name
           tk-handler
           Tk_MainLoop
           tk-main-loop
           Tcl_DoOneEvent
           tcl-do-one-event
           tk-item-canvas-id
           as-c-options
           as-c-accessors
           tk-object?
           tk-button?
           tk-canvas?
           tk-entry?
           tk-listbox?
           tk-scrollbar?
           tk-text?
           tk-toplevel?
           tk-item-canvas?
           tk_allocate_registers))

;;;-----------------------------------------------------------------------------
;;; Foreign Functions Declarations
;;;-----------------------------------------------------------------------------
(defextern eul_initialize_tk () ptr)
(defextern tk_allocate_registers (<string> ptr) ptr)
(defextern Tk_MainLoop () boolean)
(defextern Tcl_DoOneEvent (<fpi>) <fpi> "Tcl_DoOneEventAux")

;; These defexterns should be first-class
(defun tk-main-loop () (Tk_MainLoop))

(defun tcl-do-one-event (x) (Tcl_DoOneEvent x))

;;;-----------------------------------------------------------------------------
;;; Class Declarations
;;;-----------------------------------------------------------------------------
(defclass <tk-object> ()
  ((name accessor: tk-name
         keyword: name:
         default: (string-append ".tk" (symbol-name (gensym))))
   (handler accessor: tk-handler
            keyword: handler:))
  abstract?: t
  predicate: tk-object?)

(defclass <tk-button> <tk-object>() predicate: tk-button?)
(defclass <tk-label> <tk-object>())
(defclass <tk-frame> <tk-object>())
(defclass <tk-canvas> <tk-object>() predicate: tk-canvas?)
(defclass <tk-checkbutton> <tk-object>())
(defclass <tk-entry> <tk-object>() predicate: tk-entry?)
(defclass <tk-listbox> <tk-object>() predicate: tk-listbox?)
(defclass <tk-menu> <tk-object>())
(defclass <tk-menubutton> <tk-object>())
(defclass <tk-message> <tk-object>())
(defclass <tk-radiobutton> <tk-object>())
(defclass <tk-scale> <tk-object>() )
(defclass <tk-scrollbar> <tk-object>() predicate: tk-scrollbar?)
(defclass <tk-text> <tk-object>() predicate: tk-text?)
(defclass <tk-toplevel> <tk-object>() predicate: tk-toplevel?)
(defclass <tk-item-canvas> <tk-object>
  ((id accessor: tk-item-canvas-id))
  predicate: tk-item-canvas?)

;;;-----------------------------------------------------------------------------
;;; General Functions
; The next function receives a list of arguments and the clientdata argument.
; If self accessor appear in the list it will be change by the clientdata
;;;-----------------------------------------------------------------------------
(defun change-self (list clientdata)
  (let ((list-result ()))
    (do (lambda (el)
          (setq list-result
                (if (eq el self:)
                    (cons clientdata list-result)
                  (cons el list-result))))
        (reverse list))
    list-result))

(defun as-c-options (l . clientdata)
  (let (x auxlist)
    (letfuns
     ((loop (ll i res)
            (if (null? ll)
                (cons i (reverse res))
              (let ((key (car ll))
                    value)
                (cond ((tk-object? key)
                       (loop (cdr ll)
                             (+ i 1)
                             (cons (tk-name key) res)))
                      ((string? key)
                       (loop (cdr ll)
                             (+ i 1)
                             (cons key res)))
                      ((number? key)
                       (loop (cdr ll)
                             (+ i 1)
                             (cons (convert key <string>) res)))
                      (t
                       (setq value (cadr ll))
                       (cond ((or (eq key command:)
                                  (eq key yscrollcommand:)
                                  (eq key xscrollcommand:))
                              ;; We have a command.
                              (if (string? value)
                                  ;; Value is the string naming a Tcl command???
                                  (loop (cddr ll)
                                        (+ i 2)
                                        (cons
                                         value
                                         (cons (string-append "-"
                                                              (keyword-name key))
                                               res)))
                                ;; Value must be a Eulisp function
                                (let* ((function-key
                                        (symbol-name
                                         (gensym
                                          (symbol-name (function-name value)))))
                                       (rest (cddr ll))
                                       (args (if (and rest (eq (car rest) args:))
                                                 (change-self (cadr rest)
                                                              (car clientdata))
                                               ())))
                                  (tk_allocate_registers function-key
                                                         (cons value args))
                                  (loop (if (and rest
                                                 (eq (car rest) args:))
                                            (cddr rest) ;get rid of args: args
                                          rest)
                                        (+ i 2)
                                        (cons
                                         (string-append "eul_fpierpret "
                                                        function-key)
                                         (cons
                                          (string-append "-" (keyword-name key))
                                          res))))))
                             ((tk-object? value)
                              (loop (cddr ll)
                                    (+ i 2)
                                    (cons (tk-name value)
                                          (cons
                                           (string-append "-" (keyword-name key))
                                           res))))

                             (t
                              (loop (cddr ll)
                                    (+ i 2)
                                    (cons  (convert value <string>)
                                           (cons
                                            (string-append "-" (keyword-name key))
                                            res)))))))))))
     (setq x (loop l 0 ()))
     x)))

;;;-----------------------------------------------------------------------------
; The next function takes a list of accessors. The result will be a list
; of two lists. The first one of these two lists will contain the  strings
; representing the tcl-tk accessors. That is the same names without the
; colons at the end. Instead of that a % is added at the beginning.
; One special accessor could be args:. This accessor has to be the last one
; and has to be followed by a list of arguments. This list is going to be
; the second list in the result.
;;;-----------------------------------------------------------------------------
;  The next entrance:
;
;      ( x: y: W: args: (list 1 2 3))
;
;  Will produce the next result:
;
;      (("%x" "%y" "%W)(1 2 3)

(defun as-c-accessors (l)
  (let (x)
    (letfuns
     ((loop (ll res)
            (cond ((null? ll)
                   (list (reverse res) ()))
                  ((eq (car ll) args:)
                   (list (reverse res) (cadr ll)))
                  ((keyword? (car ll))
                   (loop (cdr ll)
                         (cons
                          (concatenate "%" (keyword-name (car ll))) res)))
                  (t
                   (format "Error: Some arguments are not keywords\n")
                   (flush)))))
     (setq x (loop l ())))
    x))

;;;-----------------------------------------------------------------------------
)  ;; End of module tk_general
;;;-----------------------------------------------------------------------------
