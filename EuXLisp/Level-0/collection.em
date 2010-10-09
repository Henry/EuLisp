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
  (import (root
           syntax-0
           thread
           telos
           condition
           setter
           convert
           macros
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

(define (missing-op name val)
        (error
         <collection-error>
         (if (collection? val)
             (string-append
              "missing " name " operation for collection")
           (string-append
            "not a collection in " name))
         value: val))

(define-generic (collection? x))

(define-method (collection? x) ())

(define-generic (sequence? x))

(define-method (sequence? x) ())

;;
(define-generic (accumulate (fn <function>) init c))

(define-method (accumulate (fn <function>) init c)
               (missing-op "accumulate" c))

;;
(define-generic (accumulate1 (fn <function>) c))

(define-method (accumulate1 (fn <function>) c)
               (missing-op "accumulate" c))

;;
(define-generic (all? (fn <function>) c . more))

(define-method (all? (fn <function>) c . more)
               (missing-op "all?" c))

;;
(define-generic (any? (fn <function>) c . more))

(define-method (any? (fn <function>) c . more)
               (missing-op "any?" c))

;;
(define-generic (concatenate c . more))

(define-method (concatenate c . more)
               (missing-op "concatenate" c))

;;
(define-generic (delete obj c . fn))

(define-method (delete obj c . fn)
               (missing-op "delete" c))

;;
(define-generic (do (fn <function>) c . more))

(define-method (do (fn <function>) c . more)
               (missing-op "do" c))

;;
(define-generic (element c n))

(define-method (element c n)
               (missing-op "element" c))

;;
(define-generic (setter-element c n v))

(define-method (setter-element c n v)
               (missing-op "(setter element)" c))

((setter setter) element setter-element)

;;
(define-generic (empty? c))

(define-method (empty? c)
               (missing-op "empty?" c))

;;
(define-generic (fill c o . k))

(define-method (fill c o . k)
               (missing-op "fill" c))

;;
(define-generic (map (fn <function>) c . more))

(define-method (map (fn <function>) c . more)
               (missing-op "map" c))

;;
(define-generic (member o c . test))

(define-method (member o c . test)
               (missing-op "member" c))

;;
(define-generic (remove obj c . fn))

(define-method (remove obj c . fn)
               (missing-op "remove" c))

;;
(define-generic (reverse c))

(define-method (reverse c)
               (missing-op "reverse" c))

;;
(define-generic (size x))

(define-method (size c)
               (missing-op "size" c))

;; lists
(define-method (collection? (l <list>)) t)
(define-method (sequence? (l <list>)) t)

(define-method (accumulate (fn <function>) init (l <list>))
               (acc-list fn l init))

(define (acc-list fn l sofar)
        (if (atom? l)
            sofar
          (acc-list fn (cdr l) (fn sofar (car l)))))

(define-method (accumulate1 (fn <function>) (l <list>))
               (acc1-list fn l))

(define (acc1-list fn l)
        (if (atom? l)
            ()
          (acc-list fn (cdr l) (car l))))

(define-method (all? (fn <function>) (l <list>) . more)
               (all?-list fn l more))

(define (all?-list fn l more)
        (cond ((null? more)
               (all?-1 fn l))
              ((null? (cdr more))
               (all?-2 fn l (convert (car more) <list>)))
              (t (all?-n fn (cons l (map-list (converter <list>) more))))))

(define (all?-1 fn l)
        (cond ((atom? l) t)
              ((fn (car l))
               (all?-1 fn (cdr l)))
              (t ())))

(define (any?-2 fn l1 l2)
        (cond ((or (atom? l1) (atom? l2)) t)
              ((fn (car l1) (car l2))
               (any?-2 fn (cdr l1) (cdr l2)))
              (t ())))

(define (any?-n fn ls)
        (cond ((any-atoms? ls) t)
              ((apply fn (map-list car ls))
               (any?-n fn (map-list cdr ls)))
              (t ())))

(define-method (any? (fn <function>) (l <list>) . more)
               (any?-list fn l more))

(define (any?-list fn l more)
        (cond ((null? more)
               (any?-1 fn l))
              ((null? (cdr more))
               (any?-2 fn l (convert (car more) <list>)))
              (t (any?-n fn (cons l (map-list (converter <list>) more))))))

(define (any?-1 fn l)
        (cond ((atom? l) ())
              ((fn (car l)) t)
              (t (any?-1 fn (cdr l)))))

(define (any?-2 fn l1 l2)
        (cond ((or (atom? l1) (atom? l2)) ())
              ((fn (car l1) (car l2)) t)
              (t (any?-2 fn (cdr l1) (cdr l2)))))

(define (any?-n fn ls)
        (cond ((any-atoms? ls) ())
              ((apply fn (map-list car ls)) t)
              (t (any?-n fn (map-list cdr ls)))))

(define (any-atoms? l)
        (cond ((atom? l) ())
              ((atom? (car l)) t)
              (t (any-atoms? (cdr l)))))

(define-method (concatenate (l <list>) . more)
               (if (null? more)
                   l
                 (concatenate-lists l more)))

(define (concatenate-lists l more)
        (apply append l (map-list (converter <list>) more)))

(define-method (delete obj (l <list>) . fn)
               (delete-list obj l (if (null? fn) eql (car fn))))

(define (delete-list obj l comp)
        (cond ((atom? l) (if (comp obj l) () l))
              ((comp obj (car l)) (delete-list obj (cdr l) comp))
              (t (set-cdr! l (delete-list obj (cdr l) comp)))))

(define-method (do (fn <function>) (l <list>) . more)
               (do-list fn l more))

(define (do-list fn l more)
        (cond ((null? more)
               (for-each fn l))
              ((null? (cdr more))
               (for-each fn l (convert (car more) <list>)))
              (t
               (apply for-each fn l
                      (map-list (converter <list>) more)))))

(define-method (element (l <list>) (n <integer>))
               (list-ref l n))

(define-method (setter-element (l <list>) (n <integer>) v)
               (set-car! (list-tail l n) v)
               v)

(define-method (empty? (l <list>))
               (null? l))

(define-method (fill (l <list>) o . k)
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

(define (fill-all-list l o)
        (cond ((atom? l) ())
              ((atom? (cdr l))
               (set-car! l o))
              (t (set-car! l o)
                 (fill-all-list (cdr l) o))))

(define (fill-keyed-list l o k)
        (for-each
         (lambda (key)
           (set-car! (list-tail l key) o))
         (convert k <list>)))

(define (fill-index-list l o start end)
        (fill-index-list-aux (list-tail l start) o start end))

(define (fill-index-list-aux l o index end)
        (if (<= index end)
            (progn
              (set-car! l o)
              (fill-index-list-aux (cdr l) o (+ index 1) end))))

(define-method (map (fn <function>) (c <list>) . more)
               (maplist fn c more))

(define (maplist fn c more)
        (cond ((null? more)
               (map-list fn c))
              ((null? (cdr more))
               (map-list fn c (convert (car more) <list>)))
              (t (apply map-list fn c
                        (map-list (converter <list>) more)))))

(define-method (member o (l <list>) . test)
               (memberlist o l (if (null? test) eql (car test))))

(define (memberlist o l test)
        (cond ((atom? l) ())
              ((test o (car l)) l)
              (t (memberlist o (cdr l) test))))

(define-method (remove obj (l <list>) . fn)
               (remove-list obj l (if (null? fn) eql (car fn))))

(define (remove-list obj l comp)
        (cond ((atom? l) (if (comp obj l) () l))
              ((comp obj (car l)) (remove-list obj (cdr l) comp))
              (t (cons (car l) (remove-list obj (cdr l) comp)))))

(define-method (reverse (l <list>))
               (reverse-list l))

(define-method (size (l <list>))
               (list-size l))

;; strings
(define-method (collection? (l <string>)) t)
(define-method (sequence? (l <string>)) t)

(define-method (accumulate (fn <function>) init (s <string>))
               (acc-list fn (string->list s) init))

(define-method (accumulate1 (fn <function>) (s <string>))
               (acc1-list fn (string->list s)))

(define-method (all? (fn <function>) (s <string>) . more)
               (all?-list fn (convert s <list>) more))

(define-method (any? (fn <function>) (s <string>) . more)
               (any?-list fn (convert s <list>) more))

(define-method (concatenate (s <string>) . more)
               (if (null? more)
                   s
                 (let ((result (concatenate-lists (convert s <list>) more)))
                   (if (all?-1 char? result)
                       (convert result <string>)
                     (error <collection-error>
                            "not a char in result of concatenate string"
                            value: result)))))

(define-method (delete obj (s <string>) . fn)
               (remove-seq obj s fn <string>))

(define (remove-seq obj seq fn class)
        (convert (delete-list obj
                              (convert seq <list>)
                              (if (null? fn) eql (car fn)))
                 class))

(define-method (do (fn <function>) (s <string>) . more)
               (do-list fn (convert s <list>) more))

(define-method (element (s <string>) (n <integer>))
               (string-ref s n))

(define-method (setter-element (s <string>) (n <integer>) v)
               (string-set! s n v))

(define-method (empty? (s <string>))
               (string-null? s))

(define-method (fill (s <string>) o . k)
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

(define (fill-string s o index end)
        (if (< index end)
            (progn
              (string-set! s index o)
              (fill-all-string s o (+ index 1) end))))

(define (fill-keyed-string s o k)
        (for-each
         (lambda (key)
           (string-set! s key o))
         (convert k <list>)))

(define-method (map (fn <function>) (s <string>) . more)
               (let ((result (maplist fn (convert s <list>) more)))
                 (if (all?-1 char? result)
                     (convert result <string>)
                   (error <collection-error>
                          "not a char in result of map string"
                          value: result))))

(define-method (remove obj (s <string>) . fn)
               (remove-seq obj s fn <string>))

(define-method (reverse (s <string>))
               (convert (reverse-list (convert s <list>)) <string>))

(define-method (member o (s <string>) . test)
               (if (memberlist o (convert s <list>)
                               (if (null? test) eql (car test)))
                   t
                 ()))

(define-method (size (s <string>))
               (string-size s))

;; vectors
(define-method (collection? (l <vector>)) t)
(define-method (sequence? (l <vector>)) t)

(define-method (accumulate (fn <function>) init (v <vector>))
               (acc-list fn (vector->list v) init))

(define-method (accumulate1 (fn <function>) (v <vector>))
               (acc1-list fn (vector->list v)))

(define-method (all? (fn <function>) (v <vector>) . more)
               (all?-list fn (convert v <list>) more))

(define-method (any? (fn <function>) (v <vector>) . more)
               (any?-list fn (convert v <list>) more))

(define-method (concatenate (v <vector>) . more)
               (if (null? more)
                   v
                 (convert (concatenate-lists (convert v <list>) more)
                          <vector>)))

(define-method (delete obj (v <vector>) . fn)
               (remove-seq obj v fn <vector>))

(define-method (do (fn <function>) (v <vector>) . more)
               (do-list fn (convert v <list>) more))

(define-method (element (v <vector>) (n <integer>))
               (vector-ref v n))

(define-method (setter-element (v <vector>) (n <integer>) e)
               (vector-set! v n e))

(define-method (empty? (v <vector>))
               (= (vector-size v) 0))

(define-method (fill (v <vector>) o . k)
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

(define (fill-vector v o index end)
        (if (< index end)
            (progn
              (vector-set! v index o)
              (fill-vector v o (+ index 1) end))))

(define (fill-keyed-vector v o k)
        (for-each
         (lambda (key)
           (vector-set! v key o))
         (convert k <list>)))

(define-method (map (fn <function>) (v <vector>) . more)
               (convert (maplist fn (convert v <list>) more) <vector>))

(define-method (member o (v <vector>) . test)
               (if (memberlist o (convert v <list>)
                               (if (null? test) eql (car test)))
                   t
                 ()))

(define-method (remove obj (v <vector>) . fn)
               (remove-seq obj v fn <vector>))

(define-method (reverse (v <vector>))
               (convert (reverse-list (convert v <list>)) <vector>))

(define-method (size (v <vector>))
               (vector-size v))

;; tables
(define-method (collection? (l <table>)) t)

(define-method (accumulate (fn <function>) init (t <table>))
               (acc-list fn (table-values t) init))

(define-method (accumulate1 (fn <function>) (t <table>))
               (acc1-list fn (table-values t)))

(define-method (all? (fn <function>) (t <table>) . more)
               (all?-list fn (convert t <list>) more))

(define-method (any? (fn <function>) (t <table>) . more)
               (any?-list fn (convert t <list>) more))

(define-method (concatenate (t <table>) . more)
               (if (null? more)
                   t
                 (let ((new (make-table (table-comparator t) (table-fill t))))
                   (for-each
                    (lambda (old)
                      (for-each
                       (lambda (key)
                         (table-set! new key (table-ref old key)))
                       (table-keys old)))
                    (cons t (map-list (converter <table>) more)))
                   new)))

(define-method (delete obj (t <table>) . fn)
               (if (not (or (null? fn) (eq (car fn) (table-comparator t))))
                   (error <collection-error>
                          "comparator incompatible with table in delete"
                          value: (car fn))
                 (progn
                   (table-delete t obj)
                   t)))

(define-method (do (fn <function>) (t <table>) . more)
               (do-list fn (convert t <list>) more))

(define-method (element (t <table>) key)
               (table-ref t key))

(define-method (setter-element (t <table>) key v)
               (table-set! t key v))

(define-method (empty? (t <table>))
               (= (table-size t) 0))

(define-method (fill (t <table>) o . k)
               (cond ((null? k)
                      (for-each
                       (lambda (key)
                         (table-set! t key o))
                       (table-keys t))
                      (set-table-fill! t o))
                     ((collection? (car k))
                      (fill-keyed-table t o (car k)))
                     (t (error <collection-error>
                               "no natural order for tables in fill"
                               value: k))))

(define-method (map (fn <function>) (t <table>) . more)
               (cond ((null? more)
                      (map-table fn t))
                     (t (error <collection-error>
                               "no natural order for table in map"
                               value: f))))

(define (map-table fn t)
        (let ((new (make-table (table-comparator t) (fn (table-fill t)))))
          (for-each
           (lambda (k)
             (table-set! new k (fn (table-ref t k))))
           (table-keys t))
          new))

(define-method (remove obj (t <table>) . fn)
               (if (not (or (null? fn) (eq (car fn) (table-comparator t))))
                   (error <collection-error>
                          "comparator incompatible with table in remove"
                          value: (car fn))
                 (let ((new (shallow-copy t)))
                   (table-delete new obj)
                   new)))

(define-method (reverse (t <table>))
               t)

(define-method (member o (t <table>) . test)
               (if (memberlist o (table-values t)
                               (if (null? test) eql (car test)))
                   t
                 ()))

(define-method (size (t <table>))
               (table-size t))

(define-generic (slice c s e))

(define-method (slice (str <string>) (s <int>) (e <int>))
               (substring str s e))

(define (slice-list list from to)
        (if (>= from to) ()
          (cons (list-ref list from) (slice-list list (+ from 1) to))))

(define-method (slice (list <list>) (s <int>) (e <int>))
               (slice-list list s e))

;;;-----------------------------------------------------------------------------
)  ;; End of module collection
;;;-----------------------------------------------------------------------------
