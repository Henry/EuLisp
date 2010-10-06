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
;;;  Authors: E. Ulrich Kriegel
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule small-table
  (import (tail)
   syntax (tail
            syntax-i)
   export (<small-table>
            make-small-table
            small-table-ref
            set-small-table-ref
            small-table-delete
            ;;for tests only
            table-entries
            table-key
            table-next
            table-value
            make-table-with-entries))

(%define-standard-class (<table-entry> <class>)
  <object>
  ((table-key type <object> keyword table-key reader table-key)
   (table-value type <object> keyword table-value reader table-value
                writer set-table-value)
   (table-next type <object> keyword table-next reader table-next
               writer set-table-next))
  allocation single-card
  representation pointer-to-struct
  constructor (make-table-entry table-key
                                table-value
                                table-next)
  predicate table-entry?)

(%define-standard-class (<small-table> <class>)
  <object>
  ((table-entries type <object> keyword table-entries
                  reader table-entries
                  writer set-table-entries
                  default ())
   (table-comparator type <function> keyword table-comparator
                     default eq
                     reader table-comparator))
  constructor (make-table-with-entries  table-comparator)
  allocation multiple-type-card
  representation pointer-to-struct)

(defun make-small-table comparator
  (if comparator
      (make-table-with-entries (car comparator))
    (make-table-with-entries eq)))

(defun small-table-ref
  (table key . no-entry-value)
  (small-table-ref-aux (table-entries table) key (if no-entry-value
                                                     (car no-entry-value)
                                                   no-entry-value) ))

(defun small-table-ref-aux
  (entries key default)
  (cond
    ((null? entries)
     default)
    ((eq (table-key entries)
         key)
     (table-value entries))
    (t (small-table-ref-aux (table-next entries) key default))))

(defun set-small-table-ref
  (table key value)
  (let ((entries (table-entries table)))
    (if  (null? entries)
        (progn (set-table-entries table (make-table-entry key value ()))
               value)
      (set-small-table-ref-aux entries key value (table-next entries))
      )))

(defun  set-small-table-ref-aux
  (entries key value next)
  (cond
    ((eq key (table-key entries))
     (set-table-value entries value))
    ((null? next)
     (set-table-next entries (make-table-entry key value ())))
    (t (set-small-table-ref-aux (table-next entries) key value
                                (table-next next)))))

(defun small-table-delete
  (table key)
  (let ((entries (table-entries table)))
    (if (null? entries)
        ()
      (if (eq key (table-key entries))
          (set-table-entries table (table-next entries))
        (small-table-delete-aux (table-next entries) key entries))
      )))

(defun small-table-delete-aux
  (entries key before)
  (cond ((null? entries)
         ())
        ((eq (table-key entries) key)
         (set-table-next before (table-next entries)))
        (t (small-table-delete-aux (table-next entries) key entries))))

)

;;;----------------------------------------------------------------------------
)  ;; End of module small-table
;;;-----------------------------------------------------------------------------
