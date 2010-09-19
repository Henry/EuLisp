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
;;; Title: collection consist of list, string, vector, table,
;;;  Description:
;;;  Authors: Winfried Heicking
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

#module collection-convert
(import (eulisp-kernel)
 syntax (eulisp-kernel)
 export (converter))

(defgeneric right-char? (what))

(defmethod converter ((dest <list>) (collection <list>))
  collection)

(defmethod converter ((dest <list>) (collection <vector>))
  (let ((result (cons 1 ())))
    (convert-vector-to-list collection
                            #%I0
                            (length collection)
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
          ;;            (str-size %signed-word-integer
          ;;                        (%pair-size collection))
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

(defmethod right-char? ((what <character>))
  what)

(defmethod right-char? ((what <object>))
  (print "error for convert in concatenate to string, no character")
  #\a)

;;;-----------------------------------------------------------------------------
;;;eof
;;;-----------------------------------------------------------------------------
