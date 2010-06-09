;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: tables; fixed comparator and hash-function;
;;;    assume non-relocating garbage collector!
;;;-----------------------------------------------------------------------------
(defmodule table1
  (syntax (_telos0)
   import (telos convert copy collect compare list fpi string vector)
   export (<table> tablep <simple-hash-table> simple-hash-table?
           <hash-table> hash-table?
           *min-table-entries* *table-fill-factor*
           table-entries table-fill-value table-size table-threshold
           table-hash-function table-comparator
           table-values table-keys clear-table))

;;;-----------------------------------------------------------------------------
;;; Allocation
;;;-----------------------------------------------------------------------------
  (defconstant *min-table-entries* 16)
  (defconstant *table-fill-factor* 2)
  ;; We want some fragmentation
  (defconstant *table-unused* 4)
  (defconstant *default-hash-function* hash-object)

  (defextern hash-object (ptr) <int> "eul_hash_object")

  (defclass <table> (<collection>)
    ((entries accessor: table-entries)
     (fill-value keyword: fill-value: reader: table-fill-value)
     (size default: 0 accessor: table-size)
     (threshold default: (int-binary- *min-table-entries* *table-unused*)
                accessor: table-threshold))
    ;; not abstract, so that instance creation is always (make <table> ...)
    predicate: tablep)

  (defprimclass <simple-hash-table> table-class (<table>) ()
    ;; Has to have same memory layout as <table>
    predicate: simple-hash-table?)

  (defclass <hash-table> (<table>)
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
          (if (functionp hash-fun)
              (apply make <hash-table> inits)
            (if (eq comp binary=)
                (apply make <hash-table> inits)
              (if (member1-list comp (list () eq eql))
                  (apply make <simple-hash-table>
                         (filter-keywords inits
                                          '(comparator: hash-function:)))
                (error "table initialization of ~a without hash function"
                       tab)))))
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
      (if (vectorp entries)
          (progn
            (do1-vector
             (lambda (x) (if (null x) () (setq res (cons (cdr x) res))))
             entries)
            res)
        ())))

  (defgeneric table-keys (tab))

  (defmethod table-keys ((tab <table>))
    ;; assume table entries as cons cells: (key . value)!
    (let ((entries (table-entries tab))
          (res ()))
      (if (vectorp entries)
          (progn
            (do1-vector
             (lambda (x) (if (null x) () (setq res (cons (car x) res))))
             entries)
            res)
        ())))

  (defmethod table-keys ((tab <simple-hash-table>))
    (error "keys of table ~a not accessable" tab))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
