;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EL-in-CL: table
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

#module table
(import (eulisp-kernel
         (only (make-hash-table
                make-instance
                values
                gethash
                typep
                eql
                remhash
                clrhash
                maphash)
               common-lisp))

 syntax (eulisp-kernel
         (rename
          ((defun cl:defun) (defmethod cl:defmethod))
          common-lisp)
         (only (&optional
                defclass
                setf
                defsetf)
               common-lisp))
 export (<table>
         make-table
         table-ref
         setter-table-ref
         table?
         table-delete
         clear-table))

(defclass <table>
  ()
  ((hash-table :reader table-hash-table-reader
               :writer table-hash-table-writer)
   (comparator :initarg  :comparator)
   (fill-value :initarg :fill-value
               :initform ()
               :reader ?fill-value)
   (hash-function :initarg :hash-function
                  :initform :hash-function
                  :reader ?hash-function)
   ))

(cl:defun make-table
          (&optional (comparator #'eql) (fill-value ())
                     (hash-function ()))
          (let ((tbl (make-instance '<table>
                                    :comparator comparator
                                    :fill-value fill-value
                                    :hash-function hash-function))
                (htbl (make-hash-table :test comparator)))
            (table-hash-table-writer htbl tbl)
            ;;    (setf (slot-value tbl 'comparator) comparator)
            tbl))

(cl:defmethod table-ref
              ((table <table>) key-obj)
              (values (gethash key-obj
                               (table-hash-table-reader table)
                               (?fill-value table))))

(cl:defmethod setter-table-ref ((table <table>) key value)
              (setf (gethash key (table-hash-table-reader table)) value))

(defsetf table-ref (table key) (value)
  `(setter-table-ref ,table ,key ,value))

(defun table? (o)
  (typep o '<table>))

(defmethod table-delete ((table <table>) key)
  (remhash key (table-hash-table-reader table)))

(defmethod clear-table ((table <table>))
  (clrhash (table-hash-table-reader table))table)

(defmethod map-table
  (function (table <table>))
  (maphash function (table-hash-table-reader table)))

#module-end
