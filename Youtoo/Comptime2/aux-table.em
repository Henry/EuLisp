;;; Copyright 1997 A. Kind & University of Bath
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
;;; Title: Access tables
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
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
      (do1-vector (lambda (entry) (print entry (car ss) nl))))))
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
)  ;; End of module aux-table
;;;-----------------------------------------------------------------------------
