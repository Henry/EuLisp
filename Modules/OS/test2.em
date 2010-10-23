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
;;; Title: Test object streams
;;;  Library: serial
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule test2
  (syntax (syntax-0)
   import (level-0
           serial))

;;;-----------------------------------------------------------------------------
;;; Test values
;;;-----------------------------------------------------------------------------
(defclass <foo> ()
  ((u accessor: foo-u keyword: u: default: 34)
   (v accessor: foo-v keyword: v: default: 35)))

(defclass <bar> <foo>
  ((r accessor: bar-r keyword: r: default: 36)))

(defun baz x (print (reverse x) nl))
(defun boo x x)

(defgeneric boz (x y))
(defmethod boz ((x <fpi>) (y <string>))
  (format "method1: ~a ~a\n" x y))
(defmethod boz ((x <double>) (y <symbol>))
  (format "method1: ~a ~a\n" x y))

;;;-----------------------------------------------------------------------------
;;; Serialization tests
;;;-----------------------------------------------------------------------------
(defun my-deserialize ()
  (pprint (deserialize)))

;;  (defun Deserialize ss
;;    (let ((s (if ss (car ss) stdin))
;;        (os (make <object-stream> mode: 'r)))
;;      (spprint stderr os)
;;      (connect os s)
;;      (spprint stderr os)
;;      (let ((res (read os)))
;;      (disconnect os)
;;      res)))

(defun my-serialize (type fs)
  (let* ((os (make <object-stream> mode: 'w)))
    (spprint stderr os)
    (connect os fs)
    (spprint stderr os)
    (cond
      ((binary= type "null")
       (swrite os ()))
      ((binary= type "int")
       (swrite os 42))
      ((binary= type "double")
       (swrite os 42.123))
      ((binary= type "char")
       (swrite os #\x))
      ((binary= type "string")
       (swrite os "Hello world!"))
      ((binary= type "symbol")
       (swrite os 'foo))
      ((binary= type "keyword")
       (swrite os bar:))
      ((binary= type "cons")
       (swrite os '(42 43)))
      ((binary= type "vector")
       (swrite os #(42 #\x)))
      ((binary= type "object")
       (swrite os (make <bar>)))
      ((binary= type "function1")
       (swrite os list))
      ((binary= type "function2")
       (swrite os +))
      ((binary= type "function3")
       (swrite os baz))
      ((binary= type "function4")
       (swrite os boo))
      ((binary= type "function5")
       (swrite os binary+))
      ((binary= type "function6")
       (swrite os boz))
      ((binary= type "thread")
       (let ((thr (make <current-thread> function: (lambda (thr)
                                                     (spprint stderr thr)
                                                     (sprint stderr 42 nl)
                                                     (swrite os thr)
                                                     (sprint stderr 43 nl)))))
         (thread-start thr thr)
         (thread-value thr)))
      (t
       (sformat stderr "*** ERROR: unknown type ~a\n" type)
       (swrite os ())))
    (sflush os)
    (disconnect os)))
(if (< *argc* 2)
    (my-deserialize)
  (let ((type (vector-ref *argv* 1)))
    (my-serialize type stdout)))

;;;-----------------------------------------------------------------------------
)  ;; End of module test2
;;;-----------------------------------------------------------------------------
