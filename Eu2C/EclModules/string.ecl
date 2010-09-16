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
;;; Title: EL-in-CL: module string
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module string
(import (eulisp-kernel
         (only (code-char
                char
                string
                <string>
                subseq
                concatenate
                copy-seq
                string<
                number?
                string-downcase
                string-upcase)
               common-lisp)
         (rename ((make-string cl:make-string))
                 common-lisp)
         (only (binary<) compare-generic)
         character-generic ; as-lowercase as-uppercase
         copy-generic ; deep-copy shallow-copy
  )
 export (make-string
         string?
         string-ref        ; not in el
         string-lt         ; not in el
         string-gt         ; not in el
         string-slice      ; not in el
         string-append     ; not in el
  )
 export (;;converter
         deep-copy
         shallow-copy
         binary<
         as-lowercase
         as-uppercase)
 expose ((only (equal)
               common-lisp))
 syntax (eulisp-kernel
         (rename ((defun cl:defun))
                 common-lisp)
         (only (&optional)
               common-lisp)))

(make-eulisp-class string)

(defconstant $null-char (code-char 0))

(cl:defun make-string (n &optional (character $null-char))
          (cl:make-string n :initial-element  character))

(defun string? (string)
  (cl:stringp string))

(defun string-ref (string n)
  (char string n))

(defun string-lt (string1 string2)
  (string< string1 string2))

(defun string-gt (string1 string2)
  (string> string1 string2))

(defun string-slice (string start end)
  (subseq string start end))

(defun string-append (string1 string2)
  (concatenate 'cl:string string1 string2))


(defmethod deep-copy ((strng <string>))
  (copy-seq strng))

(defmethod shallow-copy ((strng <string>))
  (copy-seq strng))

(defmethod binary< ((strng1 <string>)(strng2 <string>))
  (if (number? (string< strng1 strng2))
      t
    ()))

(defmethod as-lowercase ((strng <string>))
  (string-downcase strng))

(defmethod as-uppercase ((strng <string>))
  (string-upcase strng))

#module-end
