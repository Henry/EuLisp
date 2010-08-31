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
;;;  Title: collection for lists
;;;  Description: collection for lists gives the functionality described in A.2
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------
(defmodule collection-list
  (import (tail
           apply
           eulisp-kernel
           ;;collection-aux
           (only (eql)
                 compare)
           basic-list
           (only (print)
                 print)
           (only (<conversion-condition>
                  error)
                 condition)
           ;;convert
           collection-generic
           collection-aux)
   syntax (tail
           syntax-0
           setf)
   export (accumulate
           accumulate1
           any?
           concatenate
           do
           element
           empty?
           fill
           map
           member
           memq-list
           memq-proper-list
           reverse
           size
           ;;converter
           ;;    generic-prin
           ;;    generic-write
           ))

;;;-----------------------------------------------------------------------------
;;; accumulate
;;;-----------------------------------------------------------------------------
(defmethod accumulate ((function <function>)
                       (object <object>)
                       (lst <cons>))
  (map-accumulate function lst object))

(defmethod accumulate ((function <function>)
                       (object <object>)
                       (lst <null>))
  object)


(defun map-accumulate (function lst res)
  (if (cons? lst)
      (map-accumulate function
                      (cdr lst)
                      (function res (car lst))
                      )
    res))

;;;------------------------------------------------------------
;;; accumulate1
;;;------------------------------------------------------------
(defmethod accumulate1 ((function <function>)
                        (lst <cons>))
  (map-accumulate function (cdr lst) (car lst)))

(defmethod accumulate1 ((function <function>)
                        (lst <null>))
  lst)

;;;-----------------------------------------------------------------------------
;;; any?
;;;-----------------------------------------------------------------------------
;;  (defmethod any? ((function <function>)
;;                   (lst <list>) . more-collections)
;;    (any?-list function lst more-collections))

(defmethod any? ((function <function>)
                 (lst <cons>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (any?-with-one-list function lst))
              ((%eq rest-list-length #%i1)
               (any?-with-two-args function lst (car more-collections)))
              (t (any?-collection function lst more-collections)))
        ))

(defmethod any? ((function <function>)
                 (lst <null>) . more-collections)
  ())

(defun any?-with-one-list (function lst)
  (if (cons? lst) ;for improper lists
      (if (function (car lst))
          t
        (any?-with-one-list function (cdr lst)))
    ()))

(defmethod any?-with-two-args
  ((function <function>)
   (lst1 <list>)
   (lst2 <list>))
  (if (and (cons? lst1) (cons? lst2))
      (if (function (car lst1) (car lst2))
          t
        (any?-with-two-args function (cdr lst1) (cdr lst2)))
    ()))

(defmethod any?-with-two-args
  ((function <function>)
   (lst1 <list>)
   (collection <object>))
  (any?-collection function lst1 (cons collection ())))

;;;-----------------------------------------------------------------------------
;;; concatenate
;;;-----------------------------------------------------------------------------
(defmethod concatenate ((lst <list>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (let ((result (cons 1 ())))
                 (concat-with-one-list lst result)
                 (cdr result)))
              ((%eq rest-list-length #%i1)
               (concat-with-two-args lst (car more-collections)))
              (t (concat-collection lst more-collections)))
        ))

(defun concat-with-one-list (lst result)
  (if (cons? lst) ;for improper lists
      (concat-with-one-list (cdr lst)
                            (progn (setf (cdr result)
                                         (cons (car lst)
                                               (cdr lst)
                                               ))
                                   (cdr result)))
    result))

(defmethod concat-with-two-args ((lst1 <null>) (lst2 <null>))
  () )

(defmethod concat-with-two-args ((lst1 <null>) (lst2 <cons>))
  (let ((result (cons 1 ())))
    (concat-with-one-list lst2 result)
    (cdr result)) )

(defmethod concat-with-two-args ((lst1 <cons>) (lst2 <null>))
  (let ((result (cons 1 ())))
    (concat-with-one-list lst1 result)
    (cdr result)) )

(defmethod concat-with-two-args ((lst1 <cons>) (lst2 <cons>))
  (let ((result (cons 1 ())))
    (concat-with-one-list
     lst2
     (concat-with-one-list lst1 result))
    (cdr result)))

(defmethod concat-with-two-args
  ((lst1 <list>) (collection <object>))
  (concat-collection lst1 (cons collection ())))

;;;-----------------------------------------------------------------------------
;;; do
;;;-----------------------------------------------------------------------------
;;  (defmethod do ((function <function>)
;;                 (lst <list>) . more-collections)
;;    do-list)

(defmethod do ((function <function>)
               (lst <cons>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (do-with-one-list function lst))
              ((%eq rest-list-length #%i1)
               (do-with-two-args function lst (car more-collections)))
              (t (do-collection function lst more-collections)))
        ))

(defmethod do ((function <function>)
               (lst <null>) . more-collections)
  ())

(defun do-with-one-list (function lst)
  (if (cons? lst) ;for improper lists
      (progn (function (car lst))
             (do-with-one-list function (cdr lst)))
    ()))

(defmethod do-with-two-args
  ((function <function>)
   (lst1 <list>)
   (lst2 <list>))
  (if (and (cons? lst1) (cons? lst2)) ;for improper lists
      (progn (function (car lst1) (car lst2))
             (do-with-two-args function (cdr lst1) (cdr lst2)))
    ()))

(defmethod do-with-two-args
  ((function <function>)
   (lst1 <list>)
   (collection <object>))
  (do-collection function lst1 (cons collection ())))

;;;-----------------------------------------------------------------------------
;;; element and (setter element)
;;;-----------------------------------------------------------------------------
(defmethod element ((lst <list>)
                    (key <int>))
  (element-list lst key))

(defmethod (setter element) ((lst <list>)
                             (key <int>)
                             (value <object>))
  (setter-element-list lst key value))

;;;-----------------------------------------------------------------------------
(%define-function (element-list-aux <object>)
  ((lst <cons>)
   (key %signed-word-integer)
   (index %signed-word-integer))
  (if (%lt index key)
      (element-list-aux (cdr lst) key (%plus #%i1 index))
    lst))

(%define-function (test-list-index <object>)
  ((max-index %signed-word-integer)
   (index %signed-word-integer))
  (if (%lt index max-index)
      t
    (progn (print 'index-for-element-list-to-great)
           ()))
  )

(defun element-list (lst key)
  (%let ((swi-key %signed-word-integer (make-swi key)))
        (if (test-list-index (%pair-length lst) swi-key)
            (car (element-list-aux lst swi-key #%i0))
          ())))

(defun setter-element-list (lst key value)
  (%let ((swi-key %signed-word-integer (make-swi key)))
        (if (test-list-index (%pair-length lst) swi-key)
            (setf (car (element-list-aux lst swi-key #%i0))
                  value)
          ())))

;;;-----------------------------------------------------------------------------
;;; empty?
;;;-----------------------------------------------------------------------------
(defmethod empty? ((lst <null>))
  t)

(defmethod empty? ((lst <cons>))
  ())

;;;-----------------------------------------------------------------------------
;;; fill
;;;-----------------------------------------------------------------------------
;;  (defmethod fill ((lst <list>)
;;                   (object <object>) ;object to fill
;;                   (start <int>)
;;                   (end <int>))
;;    (fill-list lst lst object start end))

(defmethod fill ((lst <list>)
                 (object <object>) . keys)
  (%let ((list-len %signed-word-integer (%pair-length lst))
         (rest-list-length %signed-word-integer
                           (%list-length keys)))
        (if (%eq #%i0 rest-list-length)
            (fill-list-aux lst object (%minus list-len #%i1) #%i0)
          (if (%eq #%i1 rest-list-length)
              (error "fill: collection does not have natural order"
                     <conversion-condition>)
            (%let ((start %signed-word-integer (make-swi (car keys)))
                   (end %signed-word-integer (make-swi (car (cdr keys)))))
                  (if (test-range-indizes start end list-len)
                      (fill-list-aux
                       (go-to-start-list lst start #%i0)
                       object
                       (%minus end start)
                       #%i0)
                    ())))))
  ())

(%define-function (go-to-start-list <list>)
  ((lst <list>)
   (start %signed-word-integer)
   (index %signed-word-integer))
  (if (%lt index start)
      (go-to-start-list (cdr lst) start (%plus #%i1 index))
    lst))

(%define-function (fill-list-aux %void)
  ((lst <list>)
   (object <object>)
   (n %signed-word-integer)
   (index %signed-word-integer))
  (if (%gt index n)
      ()
    (progn (setf (car lst) object)
           (fill-list-aux (cdr lst) object n (%plus index #%i1)))
    ))

;;;------------------------------------------------------------
;;; map
;;;------------------------------------------------------------

;;  (defmethod map ((function <function>)
;;                  (lst <list>) . more-collections)
;;    (map-list function lst more-collections))

(defmethod map ((function <function>)
                (lst <cons>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (let ((result (cons 1 ())))
                 (map-with-one-list function lst result)
                 (cdr result)))
              ((%eq rest-list-length #%i1)
               (map-with-two-args function lst (car more-collections) ())

               ;;            (let ((result (cons 1 ())))
               ;;              (map-with-two-args function lst
               ;;                                 (car more-collections)
               ;;                                 result)
               ;;              (cdr result))

               )
              (t (map-collection function lst more-collections)))
        ))


(defmethod map ((function <function>)
                (lst <null>) . more-collections)
  ())

;;;here with cons and reverse
;;  (defun map-with-one-list (function lst result)
;;    (if lst ;(cons? lst) for improper lists
;;      (map-with-one-list function
;;                         (cdr lst)
;;                         (cons (function (car lst))
;;                               result))
;;      (reverse result)))

;;; to avoid the reverse take this
(defun map-with-one-list (function lst result)
  (if (cons? lst) ;for improper lists
      (map-with-one-list function
                         (cdr lst)
                         (progn (setf (cdr result)
                                      (cons (function (car lst)) ()))
                                (cdr result)))
    ()))

;;  (defmethod map-with-two-args
;;             ((function <function>)
;;              (lst1 <list>)
;;              (lst2 <list>)
;;              (result <object>))
;;    (if (and (cons? lst1) (cons? lst2)) ;for improper lists
;;      (map-with-two-args function
;;                         (cdr lst1)
;;                         (cdr lst2)
;;                         (progn
;;                           (setf (cdr result)
;;                                    (cons (function (car lst1) (car lst2))
;;                                          ()))
;;                           (cdr result))
;;                         )
;;      ()))

(defmethod map-with-two-args
  ((function <function>)
   (lst1 <list>)
   (lst2 <list>)
   (not-used <object>))
  (let ((result (cons 1 ())))
    (map-with-two-args-aux function lst1 lst2 result)
    (cdr result)))

(defun map-with-two-args-aux (function lst1 lst2 result)
  (if (and (cons? lst1) (cons? lst2)) ;for improper lists
      (map-with-two-args-aux function
                             (cdr lst1)
                             (cdr lst2)
                             (progn
                               (setf (cdr result)
                                     (cons (function (car lst1) (car lst2))
                                           ()))
                               (cdr result))
                             )
    ()))

(defmethod map-with-two-args
  ((function <function>)
   (lst1 <list>)
   (collection <object>)
   (not-used <object>)
   )
  (map-collection function lst1 (cons collection ())))

;;;-----------------------------------------------------------------------------
;;; member
;;;-----------------------------------------------------------------------------
;;  (defmethod member ((object <object>)
;;                     (lst <list>)
;;                     (test <function>))
;;    (member-list object lst test))


(defmethod member ((object <object>)
                   (lst <cons>) . test)
  (let ((test-fct (if test
                      (car test)
                    eql)))
    (if (eq test-fct eq)
        (memq-list object lst)
      (member-list-aux object lst test-fct))))

(defmethod member ((object <object>)
                   (lst <null>) . test)
  ())

(defun member-list-aux (object lst test)
  (if (cons? (cdr lst))
      (if (test (car lst) object)
          lst
        (member-list-aux object (cdr lst) test))
    (if (test (car lst) object)
        lst
      ())))

(defun memq-list (object lst)
  (if (cons? (cdr lst))
      (if (eq (car lst) object)
          lst
        (memq-list object (cdr lst)))
    (if (eq (car lst) object)
        lst
      ())))

(defun memq-proper-list (object lst)
  (if lst
      (if (eq (car lst) object)
          lst
        (memq-proper-list object (cdr lst)))
    ()))

;;;-----------------------------------------------------------------------------
;;; reverse
;;;-----------------------------------------------------------------------------
(defmethod reverse ((lst <cons>))
  (reverse-list lst))

(defmethod reverse ((lst <null>))
  ())

;;;reverse-list is in collection-aux now

;;;-----------------------------------------------------------------------------
;;; size
;;;-----------------------------------------------------------------------------
(defmethod size ((lst <cons>))
  (make-fpint (%pair-length lst)))

(defmethod size ((lst <null>))
  0)

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
