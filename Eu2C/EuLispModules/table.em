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
;;;  Title: tables provide a general key to value association mechanism
;;;  Description: tables provide a general key to value association mechanism
;;;  Documentation:
;;;  Notes: write more methods for key objects (function hash)
;;;  Requires:
;;;  Problems:
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------




(defmodule table

  (import
   (tail
    ;; (only (print) print)
    (only (eql)
          compare )
    basic-list
    (only (%list-length) basic-list-0)
    table-aux
    )

   syntax
   (tail
    syntax-0
    setf)

   export
   (<table>
    tablep
    ?table-vector
    ?fill-value
    table-vector-ref
    make-table
    table-ref
    setter-table-ref
    table-delete
    map-table
    clear-table
    ;; map-table-index;only to test; gives the list of a lists of a hash index
    hash
    assoc
    assq
    ;;    generic-prin
    ;;    generic-write
    )
   )


(defgeneric (converter <table>) (object))

;;  (%define-function (%proper-list-length %signed-word-integer)
;;                    ((l <list>))
;;    (if l
;;      (%plus #%i1 (%proper-list-length (cdr l)))
;;      #%i0))


(%define-standard-class (<table-vector> <class>)
  <object>
  ((element type <list>
            reader table-vector-ref
            writer set-table-vector-ref
            default ())
   (length type %unsigned-word-integer
           default
           #%I256
           ;;warum geht das nicht???
           ;;                                   (%cast %unsigned-word-integer
           ;;                                          (make-swi $standard-table-size))
           ))
  representation pointer-to-vector
  allocation single-card
  constructor (make-vector-table)
  )


;;;-----------------------------------------------------------------------------
;;; Definition of class table
;;;-----------------------------------------------------------------------------


(%define-standard-class (<table> <class>)
  <object>
  ((table-vector
    type <table-vector>
    reader ?table-vector
    default (make-vector-table)
    ;;keyword table-vector
    )
   (comparator
    type <function>
    reader ?comparator
    ;;writer comparator-writer
    keyword comparator
    default eql)
   (fill-value ;0.99
    type <object>
    reader ?fill-value
    keyword fill-value
    default ())
   (hash-function ;0.99
    type <function>
    reader ?hash-function
    keyword hash-function
    default hash)
   )
  representation pointer-to-struct
  allocation multiple-size-card
  constructor (make-table
               ;;table-vector
               comparator
               fill-value
               hash-function)
  predicate tablep
  )



;;  (defun make-table ini-opts
;;    ;comparator fill-value hash-function
;;    (%let ((rest-list-length %signed-word-integer
;;                             (%list-length ini-opts))
;;           (comparator <function> eql)
;;           (fill-value <object> ())
;;           (hash-fct <function> hash)
;;           )
;;      (cond ((%eq rest-list-length #%i0) t)
;;            ((%eq rest-list-length #%i1)
;;             (setq comparator (car ini-opts)))
;;            ((%eq rest-list-length #%i2)
;;             (setq comparator (car ini-opts))
;;             (setq fill-value (car (cdr ini-opts))))
;;            (t
;;             (setq comparator (car ini-opts))
;;             (setq fill-value (car (cdr ini-opts)))
;;             (setq hash-fct (car (cdr (cdr ini-opts)))))
;;            )
;;      (make-table-with-vector  (make-vector-table)
;;                               comparator
;;                               fill-value
;;                               hash-fct)))



;;  (defun table-ref (table key)
;;    (%let* ((hash-list <list> (table-vector-ref
;;                               (?table-vector table)
;;                               ((?hash-function table) key)))
;;            (comp <function> (?comparator table)) ;function for funcall
;;            (hash-elt <object>
;;                      (%cast <object>
;;                             (if (eq eq comp)
;;                                 (assq key hash-list)
;;                                 (assoc key hash-list comp)))))
;;      (if hash-elt
;;          (cdr hash-elt)
;;          (?fill-value table)
;;          )
;;      ))


(defun table-ref (table key)
  (let* ((hash-list (table-vector-ref
                     (?table-vector table)
                     (%cast %unsigned-word-integer
                            ((?hash-function table) key))))
         (comp (?comparator table)) ;function for funcall
         (hash-elt (if (eq eq comp)
                       (assq key hash-list)
                     (assoc key hash-list comp))))
    (if hash-elt
        (cdr hash-elt)
      (?fill-value table)
      )
    ))


(defun setter-table-ref (table key value)
  (%let* ((table-vector <table-vector> (?table-vector table))
          (hash-index %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             ((?hash-function table) key)))
          (hash-list <list> (table-vector-ref table-vector hash-index))
          (hash-elt <object> (%cast <object>
                                    (if (eq eq (?comparator table))
                                        (assq key hash-list )
                                      (assoc key hash-list
                                             (?comparator table)
                                             )))))
         (if hash-elt
;;;reset existing element
             (setf (cdr (%cast <cons> hash-elt))
                   value)
           ;;add new element
           (set-table-vector-ref table-vector
                                 hash-index
                                 (cons (cons key value) hash-list)))
         value
         ))


(defun table-delete (table key)
  (%let* ((hash-index %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             ((?hash-function table) key)))
          (hash-list <list>
                     (table-vector-ref (?table-vector table) hash-index))
          (cmp <function> (?comparator table)) ;function for funcall
          (hash-list-len %signed-word-integer (%pair-length hash-list)))
         (cond ((%eq #%i0 hash-list-len) ())   ;the alist contains nothing, do nothing
               ((%eq #%i1 hash-list-len)       ;the alist contains only 1 element
                (if (eq cmp eq)
                    (if (%eq (%cast %signed-word-integer
                                    (car (%cast <cons>
                                                (car (%cast <cons> hash-list)))))
                             (%cast %signed-word-integer key))
                        ;;first element matched?
                        (set-table-vector-ref
                         (?table-vector table)
                         hash-index
                         ())
                      ())
                  (if (cmp
                       (car (%cast <cons> (car (%cast <cons> hash-list))))
                       key)
                      ;;first element matched?
                      (set-table-vector-ref
                       (?table-vector table)
                       hash-index
                       ())
                    ())))
               (t   ;;the alist has more than 1 element
                (if (eq cmp eq)
                    (if (eq (car (%cast <cons>
                                        (car (%cast <cons> hash-list))))
                            key)
                        (set-table-vector-ref
                         (?table-vector table)
                         hash-index
                         (cdr (%cast <cons> hash-list)))
                      (table-loop-eq table
                                     (cdr (%cast <cons> hash-list))
                                     key hash-list))
                  (if (cmp (car (%cast <cons>
                                       (car (%cast <cons> hash-list))))
                           key)
                      (set-table-vector-ref
                       (?table-vector table)
                       hash-index
                       (cdr (%cast <cons> hash-list)))
                    (table-loop table
                                (cdr (%cast <cons> hash-list))
                                key hash-list cmp)))))
         table
         )
  )


(defun table-loop (table hash-list key pointer fcn)
  (cond
    ((fcn (car (car hash-list)) key) ;only with eq
     (setf (cdr (%cast <cons> pointer))
           (cdr hash-list))
     table)
    ((null (cdr hash-list))
     table)
    (t (table-loop table (cdr hash-list) key (cdr pointer) fcn))))


(defun table-loop-eq (table hash-list key pointer)
  (cond
    ((%eq (%cast %signed-word-integer (car (car hash-list)))
          (%cast %signed-word-integer key))
     (setf (cdr (%cast <cons> pointer))
           (cdr hash-list))
     table)
    ((null (cdr hash-list))
     table)
    (t (table-loop-eq table (cdr hash-list) key (cdr pointer)))))



;;;map-table-index is only to test the hash table
;;;;it prints the whole alists of the vector elements


(%define-function (dotimes-with-elt %void)
  ((index %unsigned-word-integer)
   (upper-limit %unsigned-word-integer)
   (table <table>))
  (%let ((hash-list <list> ()))
        (if (%ge index upper-limit)
            ()
          (progn
            (setq hash-list (table-vector-ref (?table-vector table) index))
            (when hash-list
                  hash-list
                  ;;(print hash-list)          ;take this if print is ok for dotted pairs
                  )
            (dotimes-with-elt (%plus #%I1 index) upper-limit table))
          )))

(defun map-table-index (table)
  (dotimes-with-elt #%I0
                    (%cast %unsigned-word-integer
                           (make-swi $standard-table-size))
                    table))


(%define-function (dotimes-with-mapc %void)
  ((index %unsigned-word-integer)
   (upper-limit %unsigned-word-integer)
   (table <table>)
   (fcn <function>)
   ;;take <function>, if ok
   )
  (if (%ge index upper-limit)
      nil
    (progn
      (mapc fcn (table-vector-ref (?table-vector table) index))
      (dotimes-with-mapc (%plus #%I1 index) upper-limit table fcn))
    ))


(defun map-table (fcn table)
  (dotimes-with-mapc #%I0
                     (%cast %unsigned-word-integer
                            (make-swi $standard-table-size))
                     table
                     fcn)
  table)


(%define-function (dotimes-with-setf %void)
  ((index %unsigned-word-integer)
   (upper-limit %unsigned-word-integer)
   (table <table>))
  (if (%ge index upper-limit)
      nil
    (progn
      (set-table-vector-ref (?table-vector table) index
                            nil)
      (dotimes-with-setf (%plus #%I1 index) upper-limit table))
    ))


(defun clear-table (table)
  (dotimes-with-setf #%I0
                     (%cast %unsigned-word-integer
                            (make-swi $standard-table-size))
                     table)
  table)

;;;-----------------------------------------------------------------------------
;;; type schemes for type inference
;;;-----------------------------------------------------------------------------

(%annotate-function
  clear-table new-signature
  (((var0 var1)
    ((var var0) (atom <table>))
    ((var var1) (var var0)))))

(%annotate-function
  map-table new-signature
  (((var0 var1 var2)
    ((var var0) (atom <table>))
    ((var var1) (atom <function>))
    ((var var2) (var var0)))))

(%annotate-function
  table-delete new-signature
  (((var0 var1 var2)
    ((var var0) (atom <table>))
    ((var var1) (var var0))
    ((var var2) (atom <object>)))))

(%annotate-function
  table-ref new-signature
  (((var0 var1 var2)
    ((var var0) (atom <object>))
    ((var var1) (atom <table>))
    ((var var2) (atom <object>)))))

(%annotate-function
  setter-table-ref new-signature
  (((var0 var1 var2 var3)
    ((var var0) (atom <object>))
    ((var var1) (atom <table>))
    ((var var2) (atom <object>))
    ((var var3) (var var0)))))

);;; eof

