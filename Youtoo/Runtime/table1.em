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
;;; Title: Tables
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    tables; fixed comparator and hash-function;
;;    assume non-relocating garbage collector!
;;;-----------------------------------------------------------------------------

(defmodule table1
  (syntax (_telos0)
   import (telos
           condition
           convert
           copy
           collect
           compare
           list
           format
           fpi
           string
           vector)
   export (<table>
           table?
           <simple-hash-table>
           simple-hash-table?
           <hash-table>
           hash-table?
           *min-table-entries*
           *table-fill-factor*
           table-entries
           table-fill-value
           table-size
           table-threshold
           table-hash-function
           table-comparator
           table-values
           table-keys
           clear-table))

;;;-----------------------------------------------------------------------------
;;; Allocation
;;;-----------------------------------------------------------------------------
(defconstant *min-table-entries* 16)
(defconstant *table-fill-factor* 2)
;; We want some fragmentation
(defconstant *table-unused* 4)
(defconstant *default-hash-function* hash-object)

(defextern hash-object (ptr) <fpi> "eul_hash_object")

(defclass <table> <collection>
  ((entries accessor: table-entries)
   (fill-value keyword: fill-value: reader: table-fill-value)
   (size default: 0 accessor: table-size)
   (threshold default: (int-binary- *min-table-entries* *table-unused*)
              accessor: table-threshold))
  ;; not abstract, so that instance creation is always (make <table> ...)
  predicate: table?)

(defprimclass <simple-hash-table> table-class (<table>) ()
              ;; Has to have same memory layout as <table>
              predicate: simple-hash-table?)

(defclass <hash-table> <table>
  ((comparator default: eql accessor: table-comparator keyword: comparator:)
   (hash-function default: *default-hash-function*
                  accessor: table-hash-function
                  keyword: hash-function:))
  predicate: hash-table?)

;;;-----------------------------------------------------------------------------
;;; Initialization so that instance creation is always (make <table> ...)
;;;-----------------------------------------------------------------------------
(defmethod initialize ((tab <table>) inits)
  (if (eq (class-of tab) <table>)
      (let ((comp (init-list-ref inits comparator:))
            (hash-fun (init-list-ref inits hash-function:)))
        (if (function? hash-fun)
            (apply make <hash-table> inits)
          (if (eq comp binary=)
              (apply make <hash-table> inits)
            (if (member1-list comp (list () eq eql))
                (apply make <simple-hash-table>
                       (filter-keywords inits
                                        '(comparator: hash-function:)))
              (error <condition>
                      (fmt "table initialization of ~a without hash function"
                           tab))))))
    (call-next-method)))

;;;-----------------------------------------------------------------------------
;;; Clear table
;;;-----------------------------------------------------------------------------
(defgeneric clear-table (tab))

(defmethod clear-table ((tab <table>))
  ((setter table-entries) tab ())
  ((setter table-size) tab 0)
  ((setter table-threshold)
   tab (int-binary- *min-table-entries* *table-unused*))
  tab)

(defmethod reset ((tab <table>))
  (clear-table tab))

;;;-----------------------------------------------------------------------------
;;; Access
;;;-----------------------------------------------------------------------------
(defmethod size ((tab <table>))
  (table-size tab))

(defgeneric table-values (tab))

(defmethod table-values ((tab <table>))
  ;; assume table entries as cons cells: (key . value)!
  (let ((entries (table-entries tab))
        (res ()))
    (if (vector? entries)
        (progn
          (do1-vector
           (lambda (x) (if (null? x) () (setq res (cons (cdr x) res))))
           entries)
          res)
      ())))

(defgeneric table-keys (tab))

(defmethod table-keys ((tab <table>))
  ;; assume table entries as cons cells: (key . value)!
  (let ((entries (table-entries tab))
        (res ()))
    (if (vector? entries)
        (progn
          (do1-vector
           (lambda (x) (if (null? x) () (setq res (cons (car x) res))))
           entries)
          res)
      ())))

(defmethod table-keys ((tab <simple-hash-table>))
  (error <condition> (fmt "keys of table ~a not accessable" tab)))

;;;-----------------------------------------------------------------------------
)  ;; End of module table1
;;;-----------------------------------------------------------------------------
