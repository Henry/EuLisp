;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind
;;; Description: Access tables
;;;-----------------------------------------------------------------------------
(defmodule aux-table
  (syntax (_macros)
   import (level1)
   export (make-access-table
           access-table-do
           access-table-map
           access-table-keys
           access-table-values
           access-table-print
           access-table-clear
           access-table-size
           c-string-as-eul-symbol))

;;;----------------------------------------------------------------------
;;; Create access table
;;;----------------------------------------------------------------------
 (defun make-access-table preds
   (let* ((tab (apply make <table> preds))
          (fun (lambda (x) (table-ref tab x))))
       ((setter setter) fun
        (lambda (x v)
          (if x
              (progn
                ((setter table-ref) tab x v)
                v)
            tab)))
       fun))

;;;-----------------------------------------------------------------------------
;;; Access to keys and values
;;;-----------------------------------------------------------------------------
 (defun access-table-keys (x)
   (let* ((fun (setter x))
          (tab (fun () ())))
     (table-keys tab)))

 (defun access-table-values (x)
   (let* ((fun (setter x))
          (tab (fun () ())))
     (table-values tab)))

;;;-----------------------------------------------------------------------------
;;; Mapping
;;;-----------------------------------------------------------------------------
 (defun access-table-do (f x)
   (let* ((fun (setter x))
          (tab (fun () ())))
     (do1-table f tab)))

 (defun access-table-map (f x)
   (let* ((fun (setter x))
          (tab (fun () ())))
     (map1-table f tab)))

 (defextern c-string-as-eul-symbol (ptr) ptr "eul_c_str_as_eul_symbol")

;;;-----------------------------------------------------------------------------
;;; Printing
;;;-----------------------------------------------------------------------------
 (defun access-table-print (x . ss)
   (let* ((fun (setter x))
          (tab (fun () ())))
     (if (null? ss)
         (do1-vector print (table-entries tab))
       (do1-vector (lambda (entry) (print entry (car ss)))))))
;;;-----------------------------------------------------------------------------
;;; Clearing
;;;-----------------------------------------------------------------------------
 (defun access-table-clear (x)
   (let* ((fun (setter x))
          (tab (fun () ())))
     (clear-table tab)))
;;;-----------------------------------------------------------------------------
;;; Size
;;;-----------------------------------------------------------------------------
 (defun access-table-size (x)
   (let* ((fun (setter x))
          (tab (fun () ())))
     (table-size tab)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
