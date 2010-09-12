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
;;;  Description: collection for tables gives the functionality described in A.2
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems: accumulate accumulate1 any? concatenate do fill map
;;    are ***not yet treated***
;;    ???reverse-table should copy the table???
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------

(defmodule collection-table

  (import
   (tail
    apply
    eulisp-kernel
    (only (eql)
          compare)
    (only (binary+ binary<)
          int)
    (only (print) print)
    (only ($standard-table-size
           ) table-aux)
    (only (<string>)
          string-ii)
    (only (<vector>
           primitive-vector-size
           primitive-vector-ref)
          vector)
    (only (element size) collection-vector)
    (only (element size) collection-string)
    collection-generic
    collection-aux
    table
    )

   syntax
   (tail
    syntax-0)

   export
   (<table>
    make-table
    setter-table-ref
    table-delete
    accumulate
    accumulate1
    any?
    do
    ;;any?-table
    concatenate
    ;;do-table
    element
    empty?
    fill
    map
    ;;map-tab
    ;; (map-table is already in table)
    member
    ;;reverse-table
    reverse
    size
    ;;converter
    ;;    generic-print
    ;;    generic-write
    )
   )




;;;(deflocal standard-size-of-hash-table 4096) ;import from table-aux


(defun contains-seqs (lst)
  (if lst
      (if (table? (car lst))
          (contains-seqs (cdr lst))
        t)
    ()))


;;;------------------------------------------------------------
;;; accumulate
;;;------------------------------------------------------------------

(defmethod accumulate ((function <function>)
                       (object <object>)
                       (table <table>))
  (accumulate-table function table object 0))



(defun accumulate-table (function table res index)
  (let ((tab-res (table-ref table index)))
    (if (eq tab-res (?fill-value table))
        res
      (accumulate-table function
                        table
                        (function res tab-res)
                        (binary+ index 1)))
    ))


;;------------------------------------------------------------
;;; accumulate1
;;;------------------------------------------------------------------



(defmethod accumulate1 ((function <function>)
                        (table <table>))
  (let ((tab-res (table-ref table 0)))
    (if (eq tab-res (?fill-value table))
        ()
      (accumulate-table function
                        table
                        tab-res
                        1))))



;;;------------------------------------------------------------
;;; any?
;;;------------------------------------------------------------------

(defmethod any? ((function <function>)
                 (table <table>) . more-collections)
  (%let ((rest-list-size %signed-word-integer
                           (%list-size more-collections)))
        (cond ((%eq rest-list-size #%i0)
               (any?-with-one-table function
                                    table
                                    #%I0
                                    (%cast %unsigned-word-integer
                                           (make-swi $standard-table-size))))
              ;;            ((%eq rest-list-size #%i1)
              ;;             (any?-with-two-args function table (car more-collections)))
              (t (any?-collection function table more-collections)))
        ))


;;    (defun any?-with-one-table (function table index)
;;      (let ((tab-res (table-ref table index)))
;;        (if (eq tab-res (?fill-value table))
;;          (if (function tab-res)
;;            t
;;            (any?-with-one-table function table (binary+ index 1)))
;;          ())))


(%define-function (any?-with-one-table <object>)
  ((function <function>)
   (table <table>)
   (index %unsigned-word-integer)
   (upper-limit %unsigned-word-integer))
  (if (%ge index upper-limit)
      ()
    (if (any?-table-aux
         function
         (table-vector-ref (?table-vector table) index))
        t
      (any?-with-one-table
       function
       table
       (%plus #%I1 index)
       upper-limit))
    ))


(defun any?-table-aux (function lst)
  (if lst
      (if (function (cdr (car lst)))
          t
        (any?-table-aux function (cdr lst)))
    ())
  )



;;;------------------------------------------------------------
;;; concatenate
;;;------------------------------------------------------------------


;;    (defmethod concatenate ((table <table>) . more-collections)
;;      (%let ((rest-list-size %signed-word-integer
;;                               (%list-size more-collections)))
;;        (cond ((%eq rest-list-size #%i0)
;;               table)
;;              ((%eq rest-list-size #%i1)
;;               (concat-with-two-args table (car more-collections)))
;;              (t (concat-collection table more-collections)))
;;        )
;;      )


(defmethod concatenate ((table <table>) . more-collections)
  (%let ((rest-list-size %signed-word-integer
                           (%list-size more-collections)))
        (cond ((%eq rest-list-size #%i0)
               table)
              ;;              ((%eq rest-list-size #%i1)
              ;;               (concat-with-two-args table (car more-collections)))
              (t (concat-collection-tab table more-collections)))
        )
  )

(defun concat-collection-tab (table more-collections)
  (if more-collections
      (progn (set-table-values (car more-collections) table)
             (concat-collection-tab table
                                    (cdr more-collections)))
    table))


(defgeneric set-table-values (collection (table <table>)))

(defmethod set-table-values ((collection <list>) (table <table>))
  (set-list-values table collection 0))

(defun set-list-values (table lst index)
  (if (cons? lst)
      (progn (setter-table-ref table index (car lst))
             (set-list-values table (cdr lst) (binary+ index 1)))
    table))


(defmethod set-table-values ((collection <vector>) (table <table>))
  (set-vector-values table
                     collection
                     (size collection)
                     0))

(defun set-vector-values (table vector max-len index)
  (if (binary< index max-len)
      (progn (setter-table-ref table
                               index
                               (element vector index))
             (set-vector-values table
                                vector
                                max-len
                                (binary+ index 1)))
    table))


(defmethod set-table-values ((collection <string>) (table <table>))
  (set-string-values table
                     collection
                     (size collection)
                     0))

(defun set-string-values (table str max-len index)
  (if (binary< index max-len)
      (progn (setter-table-ref table
                               index
                               (element str index))
             (set-string-values table
                                str
                                max-len
                                (binary+ index 1)))
    table))


;;;------------------------------------------------------------
;;; do
;;;------------------------------------------------------------------


(defmethod do ((function <function>)
               (table <table>) . more-collections)
  (%let ((rest-list-size %signed-word-integer
                           (%list-size more-collections)))
        (cond ((%eq rest-list-size #%i0)
               (do-with-one-table function
                                  table
                                  #%I0
                                  (%cast %unsigned-word-integer
                                         (make-swi $standard-table-size))))
              ;;            ((%eq rest-list-size #%i1)
              ;;             (do-with-two-args function table (car more-collections)))
              (t (do-collection function table more-collections)))
        ))



;;  (defun do-with-one-table (function table index)
;;    (let ((tab-res (table-ref table index)))
;;      (if (eq tab-res (?fill-value table))
;;        (progn (function tab-res)
;;               (do-with-one-table function table (binary+ index 1)))
;;        ())))


(%define-function (do-with-one-table <object>)
  ((function <function>)
   (table <table>)
   (index %unsigned-word-integer)
   (upper-limit %unsigned-word-integer))
  (if (%ge index upper-limit)
      ()
    (progn
      (do-table-aux
       function
       (table-vector-ref (?table-vector table) index))
      (do-with-one-table
       function
       table
       (%plus #%I1 index)
       upper-limit))
    ))

(defun do-table-aux (function lst)
  (if lst
      (progn
        (function (cdr (car lst)))
        (do-table-aux function (cdr lst)))
    ())
  )

;;;------------------------------------------------------------
;;; element
;;;------------------------------------------------------------------


(defmethod element ((table <table>) (key <object>))
  (table-ref table key))



;;;------------------------------------------------------------
;;; (setter element)
;;;------------------------------------------------------------------


(defmethod (setter element) ((table <table>)
                             (key <object>)
                             (value <object>))
  (setter-table-ref table key value))


;;;------------------------------------------------------------
;;; empty?
;;;------------------------------------------------------------------

(defmethod empty? ((table <table>))
  (dotimes-with-elt1 #%I0
                     (%cast %unsigned-word-integer
                            (make-swi $standard-table-size))
                     table))


(%define-function (dotimes-with-elt1 <object>)
  ((index %unsigned-word-integer)
   (upper-limit %unsigned-word-integer)
   (table <table>))
  (if (%ge index upper-limit)
      t
    (if (table-vector-ref (?table-vector table) index)
        ()
      (dotimes-with-elt1 (%plus #%I1 index) upper-limit table))))



;;;------------------------------------------------------------
;;; fill
;;;------------------------------------------------------------


(defmethod fill ((table <table>)
                 (object <object>) . keys)
  (%let ((rest-list-size %signed-word-integer
                           (%list-size keys)))
        (if (%eq #%i0 rest-list-size)
            (progn
              (print "no keys for table ")
              ())
          (if (%eq #%i1 rest-list-size)
              (fill-table-aux table object (car keys))
            (print "to many arguments for table"))))
  )


(defgeneric fill-table-aux (table object keys-of-collection))

(defmethod fill-table-aux
  ((table <table>)
   (object <object>)
   (keys-of-collection <list>))
  (if keys-of-collection
      (progn (setter-table-ref table (car keys-of-collection) object)
             (fill-table-aux table object (cdr keys-of-collection)))
    ()))

(defmethod fill-table-aux
  ((table <table>)
   (object <object>)
   (keys-of-collection <vector>))
  (fill-table-aux-v table
                    object
                    keys-of-collection
                    (primitive-vector-size keys-of-collection)
                    #%I0))

(%define-function (fill-table-aux-v <object>)
  ((table <table>)
   (object <object>)
   (keys-of-collection <vector>)
   (max-size %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index max-size)
      (progn (setter-table-ref
              table
              (primitive-vector-ref keys-of-collection index)
              object)
             (fill-table-aux-v table
                               object
                               keys-of-collection
                               max-size
                               (%plus index #%I1)))
    ()))


;;;------------------------------------------------------------

(defmethod map ((function <function>)
                (table <table>) . more-collections)
  (%let ((rest-list-size %signed-word-integer
                           (%list-size more-collections)))
        (cond ((%eq rest-list-size #%i0)
               (map-with-one-table function
                                   table
                                   (make-table eql () hash)
                                   #%I0
                                   (%cast %unsigned-word-integer
                                          (make-swi $standard-table-size))))
              ;;            ((%eq rest-list-size #%i1)
              ;;             (map-with-two-args function table (car more-collections) ()))
              (t (map-collection function table more-collections)))
        ))


;;  (defun map-with-one-table (function table index result)
;;    (let ((tab-res (table-ref table index)))
;;      (if (eq tab-res (?fill-value table))
;;        (progn (setter-table-ref
;;                result
;;                index
;;                (function (table-ref table index)))
;;               (map-with-one-table function table
;;                                   (binary+ index 1) result))
;;        result)))


(%define-function (map-with-one-table <object>)
  ((function <function>)
   (table <table>)
   (new-table <table>)
   (index %unsigned-word-integer)
   (upper-limit %unsigned-word-integer))
  (if (%ge index upper-limit)
      new-table
    (progn
      (map-table-aux
       function
       (table-vector-ref (?table-vector table) index)
       new-table)
      (map-with-one-table
       function
       table
       new-table
       (%plus #%I1 index)
       upper-limit))
    ))

(defun map-table-aux (function lst new-table)
  (if lst
      (progn (setter-table-ref new-table
                               (car (car lst))
                               (function (cdr (car lst))))
             (map-table-aux function (cdr lst) new-table))
    ()))


;;;------------------------------------------------------------
;;; member
;;;------------------------------------------------------------


(defmethod member ((object <object>)
                   (table <table>) . test)
  (dotimes-with-member1
   #%I0
   (%cast %unsigned-word-integer
          (make-swi $standard-table-size))
   table
   object
   (if test
       (car test)
     eql)
   ))



(%define-function (dotimes-with-member1 <object>)
  ((index %unsigned-word-integer)
   (upper-limit %unsigned-word-integer)
   (table <table>)
   (object <object>)
   (test <function>))
  (if (%ge index upper-limit)
      ()
    (if (dotimes-with-member-aux
         object
         (table-vector-ref (?table-vector table) index)
         test)
        t
      (dotimes-with-member1 (%plus #%I1 index)
                            upper-limit table object test))))

(defun dotimes-with-member-aux (object alist test)
  (if alist
      (if
          (test (cdr (car alist)) object)
          t
        (dotimes-with-member-aux object (cdr alist) test))
    ()))



;;;------------------------------------------------------------
;;; reverse
;;;------------------------------------------------------------


(defmethod reverse ((table <table>))
  table)




;;;------------------------------------------------------------
;;; size
;;;------------------------------------------------------------


(defmethod size ((table <table>))
  (make-fpint
   (%cast %signed-word-integer
          (dotimes-with-size
           #%I0
           (%cast %unsigned-word-integer
                  (make-swi $standard-table-size))
           table
           #%I0))))



(%define-function (dotimes-with-size %unsigned-word-integer)
  ((index %unsigned-word-integer)
   (upper-limit %unsigned-word-integer)
   (table <table>)
   (leng %unsigned-word-integer))
  (if (%ge index upper-limit)
      leng
    (dotimes-with-size
     (%plus #%I1 index)
     upper-limit table
     (%plus leng
            (%cast %unsigned-word-integer
                   (%pair-size (table-vector-ref (?table-vector table) index)))))))




)

;;;eof
