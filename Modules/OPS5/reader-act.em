;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : reader-act.em
;;; Date   :  8 Aug 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Functions to set up actions
;;;-----------------------------------------------------------------------------
(defmodule reader-act
  (syntax (macros macros-tag)
   import (level1 basic prod action ops-out))

(defconstant ops-warn format)

;;;-----------------------------------------------------------------------------
;;; read-action
; Process an action
;;;-----------------------------------------------------------------------------
(defun read-action (reader action prod)
  ;;(sformat ops-out "action: ~a~%" action)
  (let* ((type (car action))
         (act (cond
                ((eql type 'make) (read-make-action (cdr action)))
                ((eql type 'remove) (read-remove-action (cdr action)))
                ((eql type 'modify) (read-modify-action (cdr action)))
                ((eql type 'write) (read-write-action (cdr action)))
                ((eql type 'bind) (read-bind-action (cdr action)))
                ((eql type 'halt) (read-halt-action (cdr action)))
                (t (ops-warn "Action ~a not supported~%" type)))))
    (set-prod-actions prod (append (prod-actions prod) (list act))))
  reader)

(defun read-make-action (action)
  (sformat ops-out "make: ~a~%" action)
  (make-make-action (car action) (cdr action)))

(defun read-remove-action (action)
  ;;(sformat ops-out "remove: ~a~%" action)
  (make-remove-action action))

(defun read-modify-action (action)
  ;;(sformat ops-out "modify: ~a~%" action)
  (make-modify-action (car action) (cdr action)))

(defun read-write-action (action)
  ;;(sformat ops-out "write: ~a~%" action)
  (make-write-action action))

(defun read-bind-action (action)
  ;;(sformat ops-out "bind: ~a~%" action)
  (make-bind-action (car action) (cadr action)))

(defun read-halt-action (action)
  ;;(sformat ops-out "halt: ~a~%" action)
  (make-halt-action))
(export read-action)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
