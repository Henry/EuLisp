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
;;; Title: Collection conversion
;;;  Authors: Winfried Heicking
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule collection-convert
  (import (apply
           tail
           eulisp-kernel
           (only (make-string
                  string-pointer
                  allocate-%string)
                 string-ii)
           (only (strlen)
                 c-string-interface)
           (only (print)
                 print)
           basic-list
           (only (list)
                 pair)
           (only (<string>)
                 string-i)
           (only (binary+)
                 int)
           (only (eql)
                 compare)
           (only (<conversion-condition>
                  error)
                 condition)
           character
           vector
           table
           collection-i
           convert)
   syntax (tail
           apply
           syntax-0
           setf)
   export (converter
           convert))

(defmethod (converter <list>) ((collection <list>))
  collection)

(defmethod (converter <list>) ((collection <vector>))
  (let ((result (cons 1 ())))
    (convert-vector-to-list collection
                            #%I0
                            (primitive-vector-size collection)
                            result)
    (cdr result)))

(%define-function (convert-vector-to-list <list>)
  ((vec <vector>)
   (index %unsigned-word-integer)
   (vec-size %unsigned-word-integer)
   (result <list>))
  (if (%lt index vec-size)
      (convert-vector-to-list
       vec
       (%plus index #%I1)
       vec-size
       (progn (setf (cdr result)
                    (cons
                     (primitive-vector-ref vec index)
                     ()))
              (cdr result)))
    result))

(defmethod (converter <list>) ((collection <string>))
  (let ((result (cons 1 ())))
    (convert-string-to-list collection
                            #%I0
                            (primitive-string-size collection)
                            result)
    (cdr result)))

(%define-function (convert-string-to-list <list>)
  ((str <string>)
   (index %unsigned-word-integer)
   (str-size %unsigned-word-integer)
   (result <list>))
  (if (%lt index str-size)
      (convert-string-to-list
       str
       (%plus index #%I1)
       str-size
       (progn (setf (cdr result)
                    (cons
                     (string-ref-u str index)
                     ()))
              (cdr result)))
    result))

(defmethod (converter <list>) ((collection <table>))
  (convert-table-to-list collection 0))

(defun convert-table-to-list (table index)
  (let ((tab-res (table-ref table index)))
    (if (eq tab-res (?fill-value table))
        ()
      (cons tab-res (convert-table-to-list table (binary+ index 1)))
      )))

(defmethod (converter <vector>) ((collection <vector>))
  collection)

(defmethod (converter <vector>) ((collection <list>))
  (%let (
         ;;           (vec-size %unsigned-word-integer
         ;;                        (%pair-size collection))
         (result-vector <vector>
                        (make-uninitialized-vector
                         (%cast %unsigned-word-integer
                                (%pair-size collection)))))
        (convert-list-to-vector collection result-vector #%I0)))

(%define-function (convert-list-to-vector <vector>)
  ((lst <list>)
   (result-vector <vector>)
   (start %unsigned-word-integer))
  (if (cons? lst)
      (progn
        (setf-primitive-vector-ref result-vector
                                   start
                                   (car lst))
        (convert-list-to-vector (cdr lst)
                                result-vector
                                (%plus start #%I1))
        )
    result-vector))

(defmethod (converter <vector>) ((collection <string>))
  (%let* ((vec-size %unsigned-word-integer
                      (primitive-string-size collection))
          (result-vector <vector>
                         (make-uninitialized-vector
                          vec-size)))
         (convert-string-to-vector collection result-vector vec-size #%I0)))

(%define-function (convert-string-to-vector <vector>)
  ((str <string>)
   (result-vector <vector>)
   (str-size %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index str-size)
      (progn
        (setf-primitive-vector-ref result-vector
                                   index
                                   (string-ref-u str index))
        (convert-string-to-vector str
                                  result-vector
                                  str-size
                                  (%plus index #%I1)))
    result-vector))

(defmethod (converter <vector>) ((collection <table>))
  (convert (convert collection <list>) <vector>))

(defmethod (converter <string>) ((collection <string>))
  collection)

(defmethod (converter <string>) ((collection <vector>))
  (%let* ((str-size %unsigned-word-integer
                      (primitive-vector-size collection))
          (result-string <string>
                         (make-string
                          (allocate-%string
                           (%plus #%i1
                                  (%cast %signed-word-integer
                                         str-size))))))
         (convert-vector-to-string collection result-string str-size #%I0)))

(%define-function (convert-vector-to-string <string>)
  ((vec <vector>)
   (result-string <string>)
   (vec-size %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index vec-size)
      (progn
        (primitive-setter-string-ref
         result-string
         index
         (%cast %unsigned-byte-integer
                (char-code
                 (right-char? (primitive-vector-ref vec index)))))
        (convert-vector-to-string vec
                                  result-string
                                  vec-size
                                  (%plus index #%I1)))
    result-string))

(defmethod (converter <string>) ((collection <list>))
  (%let* (
;;;            (str-size %signed-word-integer
;;;                        (%pair-size collection))
          (result-string <string>
                         (make-string
                          (allocate-%string
                           (%plus #%i1 (%pair-size collection))))))
         (convert-list-to-string collection result-string #%I0)))

(%define-function (convert-list-to-string <string>)
  ((lst <list>)
   (result-string <string>)
   (index %unsigned-word-integer))
  (if (cons? lst)
      (progn
        (primitive-setter-string-ref result-string
                                     index
                                     (%cast %unsigned-byte-integer
                                            (char-code
                                             (right-char? (car lst)))))
        (convert-list-to-string (cdr lst)
                                result-string
                                (%plus index #%I1)))
    result-string))

(defmethod (converter <string>) ((collection <table>))
  (convert (convert collection <list>) <string>))

(defmethod (converter <table>) ((collection <table>))
  collection)

(defmethod (converter <table>) ((collection <list>))
  (let ((table (make-table eql () hash)))
    (convert-list-to-table collection table 0)))

(defun convert-list-to-table (lst table index)
  (if (cons? lst)
      (progn (setter-table-ref table index (car lst))
             (convert-list-to-table (cdr lst) table (binary+ index 1)))
    table))

(defmethod (converter <table>) ((collection <vector>))
  (let ((table (make-table eql () hash)))
    (convert-vector-to-table collection
                             table
                             (primitive-vector-size collection) #%I0)))

(%define-function (convert-vector-to-table <table>)
  ((vec <vector>)
   (result-table <table>)
   (vec-size %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index vec-size)
      (progn
        (setter-table-ref
         result-table
         (make-fpint (%cast %signed-word-integer index))
         (primitive-vector-ref vec index))
        (convert-vector-to-table vec
                                 result-table
                                 vec-size
                                 (%plus index #%I1)))
    result-table))


(defmethod (converter <table>) ((collection <string>))
  (let ((table (make-table eql () hash)))
    (convert-string-to-table collection
                             table
                             (primitive-string-size collection)
                             #%I0)))

(%define-function (convert-string-to-table <table>)
  ((str <string>)
   (result-table <table>)
   (str-size %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index str-size)
      (progn
        (setter-table-ref result-table
                          (make-fpint (%cast %signed-word-integer index))
                          (string-ref-u str index))
        (convert-string-to-table str
                                 result-table
                                 str-size
                                 (%plus index #%I1)))
    result-table))

(defgeneric right-char? (what))

(defmethod right-char? ((what <character>))
  what)

(defmethod right-char? ((what <object>))
  (error <conversion-condition> "convert: no characters for string "))

;;;-----------------------------------------------------------------------------
)  ;; End of module collection-convert
;;;-----------------------------------------------------------------------------
