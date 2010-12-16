;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp Level-0 collection module
;;;  Problems:
;;    * Many are inefficient and could do with a rewrite.
;;    * The semantics of some (tables, in particular) may be awry.
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule collection
  (syntax (syntax-0)
   import (root
           thread
           telos
           condition
           setter
           convert
           copy
           list)
   export (<collection-condition>
           <collection-error>
           collection?
           sequence?
           accumulate
           accumulate1
           all?
           any?
           concatenate
           delete
           do
           element
           empty?
           fill
           map
           member
           remove
           reverse
           size
           slice))

(defcondition <collection-condition> ()
  ()
  abstract?: t)

(defcondition <collection-error> <collection-condition>
  ((value default: "no-value")))

(defun missing-op (name val)
        (error
         <collection-error>
         (if (collection? val)
             (string-append
              "missing " name " operation for collection")
           (string-append
            "not a collection in " name))
         value: val))

(defgeneric collection? (x))

(defmethod collection? (x) ())

(defgeneric sequence? (x))

(defmethod sequence? (x) ())

;;
(defgeneric accumulate ((fn <function>) init c))

(defmethod accumulate ((fn <function>) init c)
               (missing-op "accumulate" c))

;;
(defgeneric accumulate1 ((fn <function>) c))

(defmethod accumulate1 ((fn <function>) c)
               (missing-op "accumulate" c))

;;
(defgeneric all? ((fn <function>) c . more))

(defmethod all? ((fn <function>) c . more)
               (missing-op "all?" c))

;;
(defgeneric any? ((fn <function>) c . more))

(defmethod any? ((fn <function>) c . more)
               (missing-op "any?" c))

;;
(defgeneric concatenate (c . more))

(defmethod concatenate (c . more)
               (missing-op "concatenate" c))

;;
(defgeneric delete (obj c . fn))

(defmethod delete (obj c . fn)
               (missing-op "delete" c))

;;
(defgeneric do ((fn <function>) c . more))

(defmethod do ((fn <function>) c . more)
               (missing-op "do" c))

;;
(defgeneric element (c n))

(defmethod element (c n)
               (missing-op "element" c))

;;
(defgeneric setter-element (c n v))

(defmethod setter-element (c n v)
               (missing-op "(setter element)" c))

((setter setter) element setter-element)

;;
(defgeneric empty? (c))

(defmethod empty? (c)
               (missing-op "empty?" c))

;;
(defgeneric fill (c o . k))

(defmethod fill (c o . k)
               (missing-op "fill" c))

;;
(defgeneric map ((fn <function>) c . more))

(defmethod map ((fn <function>) c . more)
               (missing-op "map" c))

;;
(defgeneric member (o c . test))

(defmethod member (o c . test)
               (missing-op "member" c))

;;
(defgeneric remove (obj c . fn))

(defmethod remove (obj c . fn)
               (missing-op "remove" c))

;;
(defgeneric reverse (c))

(defmethod reverse (c)
               (missing-op "reverse" c))

;;
(defgeneric size (x))

(defmethod size (c)
               (missing-op "size" c))

;; lists
(defmethod collection? ((l <list>)) t)
(defmethod sequence? ((l <list>)) t)

(defmethod accumulate ((fn <function>) init (l <list>))
               (acc-list fn l init))

(defun acc-list (fn l sofar)
        (if (atom? l)
            sofar
          (acc-list fn (cdr l) (fn sofar (car l)))))

(defmethod accumulate1 ((fn <function>) (l <list>))
               (acc1-list fn l))

(defun acc1-list (fn l)
        (if (atom? l)
            ()
          (acc-list fn (cdr l) (car l))))

(defmethod all? ((fn <function>) (l <list>) . more)
               (all?-list fn l more))

(defun all?-list (fn l more)
        (cond ((null? more)
               (all?-1 fn l))
              ((null? (cdr more))
               (all?-2 fn l (convert (car more) <list>)))
              (t (all?-n fn (cons l (map-list (converter <list>) more))))))

(defun all?-1 (fn l)
        (cond ((atom? l) t)
              ((fn (car l))
               (all?-1 fn (cdr l)))
              (t ())))

(defun any?-2 (fn l1 l2)
        (cond ((or (atom? l1) (atom? l2)) t)
              ((fn (car l1) (car l2))
               (any?-2 fn (cdr l1) (cdr l2)))
              (t ())))

(defun any?-n (fn ls)
        (cond ((any-atoms? ls) t)
              ((apply fn (map-list car ls))
               (any?-n fn (map-list cdr ls)))
              (t ())))

(defmethod any? ((fn <function>) (l <list>) . more)
               (any?-list fn l more))

(defun any?-list (fn l more)
        (cond ((null? more)
               (any?-1 fn l))
              ((null? (cdr more))
               (any?-2 fn l (convert (car more) <list>)))
              (t (any?-n fn (cons l (map-list (converter <list>) more))))))

(defun any?-1 (fn l)
        (cond ((atom? l) ())
              ((fn (car l)) t)
              (t (any?-1 fn (cdr l)))))

(defun any?-2 (fn l1 l2)
        (cond ((or (atom? l1) (atom? l2)) ())
              ((fn (car l1) (car l2)) t)
              (t (any?-2 fn (cdr l1) (cdr l2)))))

(defun any?-n (fn ls)
        (cond ((any-atoms? ls) ())
              ((apply fn (map-list car ls)) t)
              (t (any?-n fn (map-list cdr ls)))))

(defun any-atoms? (l)
        (cond ((atom? l) ())
              ((atom? (car l)) t)
              (t (any-atoms? (cdr l)))))

(defmethod concatenate ((l <list>) . more)
               (if (null? more)
                   l
                 (concatenate-lists l more)))

(defun concatenate-lists (l more)
        (apply append l (map-list (converter <list>) more)))

(defmethod delete (obj (l <list>) . fn)
               (delete-list obj l (if (null? fn) eql (car fn))))

(defun delete-list (obj l comp)
        (cond ((atom? l) (if (comp obj l) () l))
              ((comp obj (car l)) (delete-list obj (cdr l) comp))
              (t (set-cdr l (delete-list obj (cdr l) comp)))))

(defmethod do ((fn <function>) (l <list>) . more)
               (do-list fn l more))

(defun do-list (fn l more)
        (cond ((null? more)
               (for-each fn l))
              ((null? (cdr more))
               (for-each fn l (convert (car more) <list>)))
              (t
               (apply for-each fn l
                      (map-list (converter <list>) more)))))

(defmethod element ((l <list>) (n <integer>))
               (list-ref l n))

(defmethod setter-element ((l <list>) (n <integer>) v)
               (set-car (list-tail l n) v)
               v)

(defmethod empty? ((l <list>))
               (null? l))

(defmethod fill ((l <list>) o . k)
               (cond ((null? k)
                      (fill-all-list l o))
                     ((collection? (car k))
                      (fill-keyed-list l o (car k)))
                     ((and (integer? (car k))
                           (not (atom? (cdr k)))
                           (integer? (cadr k)))
                      (fill-index-list l o (car k) (cadr k)))
                     (t (error <collection-error>
                               "bad keys in fill"
                               value: k)))
               ())

(defun fill-all-list (l o)
        (cond ((atom? l) ())
              ((atom? (cdr l))
               (set-car l o))
              (t (set-car l o)
                 (fill-all-list (cdr l) o))))

(defun fill-keyed-list (l o k)
        (for-each
         (lambda (key)
           (set-car (list-tail l key) o))
         (convert k <list>)))

(defun fill-index-list (l o start end)
        (fill-index-list-aux (list-tail l start) o start end))

(defun fill-index-list-aux (l o index end)
        (if (%<= index end)
            (progn
              (set-car l o)
              (fill-index-list-aux (cdr l) o (%+ index 1) end))))

(defmethod map ((fn <function>) (c <list>) . more)
               (maplist fn c more))

(defun maplist (fn c more)
        (cond ((null? more)
               (map-list fn c))
              ((null? (cdr more))
               (map-list fn c (convert (car more) <list>)))
              (t (apply map-list fn c
                        (map-list (converter <list>) more)))))

(defmethod member (o (l <list>) . test)
               (memberlist o l (if (null? test) eql (car test))))

(defun memberlist (o l test)
        (cond ((atom? l) ())
              ((test o (car l)) l)
              (t (memberlist o (cdr l) test))))

(defmethod remove (obj (l <list>) . fn)
               (remove-list obj l (if (null? fn) eql (car fn))))

(defun remove-list (obj l comp)
        (cond ((atom? l) (if (comp obj l) () l))
              ((comp obj (car l)) (remove-list obj (cdr l) comp))
              (t (cons (car l) (remove-list obj (cdr l) comp)))))

(defmethod reverse ((l <list>))
               (reverse-list l))

(defmethod size ((l <list>))
               (list-size l))

;; strings
(defmethod collection? ((l <string>)) t)
(defmethod sequence? ((l <string>)) t)

(defmethod accumulate ((fn <function>) init (s <string>))
               (acc-list fn (string->list s) init))

(defmethod accumulate1 ((fn <function>) (s <string>))
               (acc1-list fn (string->list s)))

(defmethod all? ((fn <function>) (s <string>) . more)
               (all?-list fn (convert s <list>) more))

(defmethod any? ((fn <function>) (s <string>) . more)
               (any?-list fn (convert s <list>) more))

(defmethod concatenate ((s <string>) . more)
               (if (null? more)
                   s
                 (let ((result (concatenate-lists (convert s <list>) more)))
                   (if (all?-1 char? result)
                       (convert result <string>)
                     (error <collection-error>
                            "not a char in result of concatenate string"
                            value: result)))))

(defmethod delete (obj (s <string>) . fn)
               (remove-seq obj s fn <string>))

(defun remove-seq (obj seq fn class)
        (convert (delete-list obj
                              (convert seq <list>)
                              (if (null? fn) eql (car fn)))
                 class))

(defmethod do ((fn <function>) (s <string>) . more)
               (do-list fn (convert s <list>) more))

(defmethod element ((s <string>) (n <integer>))
               (string-ref s n))

(defmethod setter-element ((s <string>) (n <integer>) v)
               (string-set s n v))

(defmethod empty? ((s <string>))
               (string-null? s))

(defmethod fill ((s <string>) o . k)
               (cond ((null? k)
                      (fill-string s o 0 (string-size s)))
                     ((collection? (car k))
                      (fill-keyed-string s o (car k)))
                     ((and (integer? (car k))
                           (not (atom? (cdr k)))
                           (integer? (cadr k)))
                      (fill-string s o (car k) (cadr k)))
                     (t (error <collection-error>
                               "bad keys in fill"
                               value: k)))
               ())

(defun fill-string (s o index end)
        (if (%< index end)
            (progn
              (string-set s index o)
              (fill-all-string s o (%+ index 1) end))))

(defun fill-keyed-string (s o k)
        (for-each
         (lambda (key)
           (string-set s key o))
         (convert k <list>)))

(defmethod map ((fn <function>) (s <string>) . more)
               (let ((result (maplist fn (convert s <list>) more)))
                 (if (all?-1 char? result)
                     (convert result <string>)
                   (error <collection-error>
                          "not a char in result of map string"
                          value: result))))

(defmethod remove (obj (s <string>) . fn)
               (remove-seq obj s fn <string>))

(defmethod reverse ((s <string>))
               (convert (reverse-list (convert s <list>)) <string>))

(defmethod member (o (s <string>) . test)
               (if (memberlist o (convert s <list>)
                               (if (null? test) eql (car test)))
                   t
                 ()))

(defmethod size ((s <string>))
               (string-size s))

;; vectors
(defmethod collection? ((l <vector>)) t)
(defmethod sequence? ((l <vector>)) t)

(defmethod accumulate ((fn <function>) init (v <vector>))
               (acc-list fn (vector->list v) init))

(defmethod accumulate1 ((fn <function>) (v <vector>))
               (acc1-list fn (vector->list v)))

(defmethod all? ((fn <function>) (v <vector>) . more)
               (all?-list fn (convert v <list>) more))

(defmethod any? ((fn <function>) (v <vector>) . more)
               (any?-list fn (convert v <list>) more))

(defmethod concatenate ((v <vector>) . more)
               (if (null? more)
                   v
                 (convert (concatenate-lists (convert v <list>) more)
                          <vector>)))

(defmethod delete (obj (v <vector>) . fn)
               (remove-seq obj v fn <vector>))

(defmethod do ((fn <function>) (v <vector>) . more)
               (do-list fn (convert v <list>) more))

(defmethod element ((v <vector>) (n <integer>))
               (vector-ref v n))

(defmethod setter-element ((v <vector>) (n <integer>) e)
               (vector-set v n e))

(defmethod empty? ((v <vector>))
               (%= (vector-size v) 0))

(defmethod fill ((v <vector>) o . k)
               (cond ((null? k)
                      (fill-vector v o 0 (vector-size v)))
                     ((collection? (car k))
                      (fill-keyed-vector v o (car k)))
                     ((and (integer? (car k))
                           (not (atom? (cdr k)))
                           (integer? (cadr k)))
                      (fill-vector v o (car k) (cadr k)))
                     (t (error <collection-error>
                               "bad keys in fill"
                               value: k)))
               ())

(defun fill-vector (v o index end)
        (if (%< index end)
            (progn
              (vector-set v index o)
              (fill-vector v o (%+ index 1) end))))

(defun fill-keyed-vector (v o k)
        (for-each
         (lambda (key)
           (vector-set v key o))
         (convert k <list>)))

(defmethod map ((fn <function>) (v <vector>) . more)
               (convert (maplist fn (convert v <list>) more) <vector>))

(defmethod member (o (v <vector>) . test)
               (if (memberlist o (convert v <list>)
                               (if (null? test) eql (car test)))
                   t
                 ()))

(defmethod remove (obj (v <vector>) . fn)
               (remove-seq obj v fn <vector>))

(defmethod reverse ((v <vector>))
               (convert (reverse-list (convert v <list>)) <vector>))

(defmethod size ((v <vector>))
               (vector-size v))

;; tables
(defmethod collection? ((l <table>)) t)

(defmethod accumulate ((fn <function>) init (t <table>))
               (acc-list fn (table-values t) init))

(defmethod accumulate1 ((fn <function>) (t <table>))
               (acc1-list fn (table-values t)))

(defmethod all? ((fn <function>) (t <table>) . more)
               (all?-list fn (convert t <list>) more))

(defmethod any? ((fn <function>) (t <table>) . more)
               (any?-list fn (convert t <list>) more))

(defmethod concatenate ((t <table>) . more)
               (if (null? more)
                   t
                 (let ((new (make-table (table-comparator t) (table-fill t))))
                   (for-each
                    (lambda (old)
                      (for-each
                       (lambda (key)
                         (table-set new key (table-ref old key)))
                       (table-keys old)))
                    (cons t (map-list (converter <table>) more)))
                   new)))

(defmethod delete (obj (t <table>) . fn)
               (if (not (or (null? fn) (eq (car fn) (table-comparator t))))
                   (error <collection-error>
                          "comparator incompatible with table in delete"
                          value: (car fn))
                 (progn
                   (table-delete t obj)
                   t)))

(defmethod do ((fn <function>) (t <table>) . more)
               (do-list fn (convert t <list>) more))

(defmethod element ((t <table>) key)
               (table-ref t key))

(defmethod setter-element ((t <table>) key v)
               (table-set t key v))

(defmethod empty? ((t <table>))
               (%= (table-size t) 0))

(defmethod fill ((t <table>) o . k)
               (cond ((null? k)
                      (for-each
                       (lambda (key)
                         (table-set t key o))
                       (table-keys t))
                      (set-table-fill t o))
                     ((collection? (car k))
                      (fill-keyed-table t o (car k)))
                     (t (error <collection-error>
                               "no natural order for tables in fill"
                               value: k))))

(defmethod map ((fn <function>) (t <table>) . more)
               (cond ((null? more)
                      (map-table fn t))
                     (t (error <collection-error>
                               "no natural order for table in map"
                               value: f))))

(defun map-table (fn t)
        (let ((new (make-table (table-comparator t) (fn (table-fill t)))))
          (for-each
           (lambda (k)
             (table-set new k (fn (table-ref t k))))
           (table-keys t))
          new))

(defmethod remove (obj (t <table>) . fn)
               (if (not (or (null? fn) (eq (car fn) (table-comparator t))))
                   (error <collection-error>
                          "comparator incompatible with table in remove"
                          value: (car fn))
                 (let ((new (shallow-copy t)))
                   (table-delete new obj)
                   new)))

(defmethod reverse ((t <table>))
               t)

(defmethod member (o (t <table>) . test)
               (if (memberlist o (table-values t)
                               (if (null? test) eql (car test)))
                   t
                 ()))

(defmethod size ((t <table>))
               (table-size t))

(defgeneric slice (c s e))

(defmethod slice ((str <string>) (s <fpi>) (e <fpi>))
               (substring str s e))

(defun slice-list (list from to)
        (if (%>= from to) ()
          (cons (list-ref list from) (slice-list list (%+ from 1) to))))

(defmethod slice ((list <list>) (s <fpi>) (e <fpi>))
               (slice-list list s e))

;;;-----------------------------------------------------------------------------
)  ;; End of module collection
;;;-----------------------------------------------------------------------------
