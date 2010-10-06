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
;;; Title: auxiliary functions for collections
;;;  Description: collection gives the functionality described in A.2
;;;  Authors: Winfried Heicking
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule collection-i
  (import (apply
           tail
           eulisp-kernel
           (only (string-pointer)
                 string-ii)
           basic-list
           (only (<string>)
                 string)
           character
           (only (strdup
                  strlen)
                 c-string-interface))
   syntax (tail
           apply
           syntax-i
           setf)
   export (primitive-setter-string-ref
           primitive-string-size
           primitive-string-ref
           string-ref
           string-ref-u
           string-size
           list?
           nconc
           and-aux))

;;;-----------------------------------------------------------------------------
;;; Auxillary functions for strings
;;;-----------------------------------------------------------------------------
;;  (defun setter-string-ref (string n value)
;;    (%setf (%extract (string-pointer string)
;;                     (%cast %unsigned-word-integer
;;                            (make-swi n)))
;;           (make-swi (convert-char-int value))))

(%define-function (primitive-setter-string-ref %void)
  ((string <string>)
   (n %unsigned-word-integer)
   (value %unsigned-byte-integer))
  (%setf (%extract (string-pointer string) n) value))

;;  (defun string-size (string)
;;    (make-fpint (strlen (string-pointer string))))

(%define-function (primitive-string-size %unsigned-word-integer)
  ((string <string>))
  (%cast %unsigned-word-integer (strlen (string-pointer string))))

(%define-function (primitive-string-ref %unsigned-byte-integer)
  ((string <string>)
   (n %unsigned-word-integer))
  (%cast %unsigned-byte-integer (%extract (string-pointer string) n)))

;;  (defun string-ref (string n)
;;    (convert-int-char
;;     (make-fpint
;;      (%cast %signed-word-integer
;;             (%extract (string-pointer string)
;;                       (%cast %unsigned-word-integer
;;                              (make-swi n)))))))

(%define-function (string-ref-u <character>)
  ((string <string>)
   (n %unsigned-word-integer))
  (convert-int-char
   (make-fpint
    (%cast %signed-word-integer
           (%extract (string-pointer string) n)))))

(defun string-size (string)
  (make-fpint (strlen (string-pointer string))))

(defun string-ref (string n)
  (convert-int-char
   (make-fpint
    (%cast %signed-word-integer
           (%extract (string-pointer string)
                     (%cast %unsigned-word-integer
                            (make-swi n)))))))

(defun list? (object)
  (if (cons? object)
      t
    (if (null? object)
        t

      ())))

;;(%define-function (%proper-list-size %signed-word-integer)
;;                    ((l <list>))
;;    (if l
;;      (%plus #%i1 (%proper-list-size (cdr l)))
;;      #%i0))

(defun nconc (liste element)
  (if (cdr liste)
      (nconc (cdr liste) element)
    (setf (cdr liste) element))
  liste)

(defun and-aux (a b) (if a (if b t ()) ()))

;;;-----------------------------------------------------------------------------
)  ;; End of module collection-i
;;;-----------------------------------------------------------------------------
