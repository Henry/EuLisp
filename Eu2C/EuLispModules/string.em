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
;;;  Title: Module string
;;;  Description:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule string
  (import (tail
           syntax-0
           string-ii
           convert
           symbol
           (only (binary<
                  equal)
                 compare-generic)
           (only (generic-prin
                  generic-write)
                 stream-generic)
           c-string-interface
           (only (convert-int-char)
                 character)
           (only (upper?
                  upper2lower
                  lower?
                  lower2upper)
                 char-tables)
           (only (as-lowercase
                  as-uppercase)
                 character)
           (only (deep-copy
                  shallow-copy)
                 copy))
   syntax (tail
           syntax-0)
   export (<string>
           string?
           make-string
           string-pointer
           equal
           deep-copy
           shallow-copy
           binary<
           as-lowercase
           as-uppercase
           generic-prin
           generic-write
           converter
           convert))

;;;-----------------------------------------------------------------------------
;;; converter method
;;;-----------------------------------------------------------------------------
(defmethod (converter <symbol>) ((collection <string>))
  (let ((sym (symbol-exists? collection)))
    (if sym
        sym
      (make-symbol collection))))

;;;-----------------------------------------------------------------------------
;;; binary<
;;;-----------------------------------------------------------------------------
(defmethod binary< ((string1 <string>) (string2 <string>))
  (%let* ((s1 %string (string-pointer string1))
          (s2 %string (string-pointer string2))
          (l1 %signed-word-integer (strlen s1))
          (l2 %signed-word-integer (strlen s2)))
         (if (%eq l1 l2)
             (b<-string #%i0 l1 s1 s2 ())
           (if (%gt l2 l1)
               (b<-string1 #%i0 l1 s1 s2)
             '()))))

(%define-function (b<-string <object>)
  ((idx %signed-word-integer)
   (length %signed-word-integer)
   (s1 %string)
   (s2 %string)
   (flag <object>))
  (if (%lt idx length)
      (%let ((char1 %unsigned-byte-integer (%extract s1 idx))
             (char2 %unsigned-byte-integer (%extract s2 idx)))
            (if (%lt char1 char2)
                (b<-string (%plus idx #%i1) length s1 s2 t)
              (if (%eq char1 char2)
                  (b<-string (%plus idx #%i1) length s1 s2 (if flag t ()))
                '())))
    (if flag
        't
      '())))

(%define-function (b<-string1 <object>)
  ((idx %signed-word-integer)
   (length %signed-word-integer)
   (s1 %string)
   (s2 %string))
  (if (%lt idx length)
      (if (%le (%extract s1 idx) (%extract s2 idx))
          (b<-string1 (%plus idx #%i1) length s1 s2)
        '())
    't))

;;;-----------------------------------------------------------------------------
;;; as-lowercase
;;;-----------------------------------------------------------------------------
(defmethod as-lowercase ((string1 <string>))
  (%let* ((string2 <string>
                   (make-string (duplicate-%string (string-pointer string1))))
          (s2 %string (string-pointer string2))
          (l2 %signed-word-integer (strlen s2)))
         (as-lowercase-1 #%i0 l2 s2)
         string2))

(%define-function (as-lowercase-1 %void)
  ((idx %signed-word-integer)
   (length %signed-word-integer)
   (s2 %string))
  (if (%le idx length)
      (%let ((ch %signed-word-integer
                 (%cast %signed-word-integer (%extract s2 idx))))  ;;RR
            (if (upper? ch)
                (%setf (%extract s2 idx)
                       (%cast %unsigned-byte-integer (upper2lower
                                                      ch)))
              ())
            (as-lowercase-1 (%plus idx #%i1) length s2))
    ()))

;;;-----------------------------------------------------------------------------
;;; as-uppercase
;;;-----------------------------------------------------------------------------
(defmethod as-uppercase ((string1 <string>))
  ;;(%define-function (as-uppercase <string>)
  ;;                  ((string1 <string>))
  (%let* ((string2 <string>
                   (make-string (duplicate-%string (string-pointer string1))))
          (s2 %string (string-pointer string2))
          (l2 %signed-word-integer (strlen s2)))
         (as-uppercase-1 #%i0 l2 s2)
         string2))

(%define-function (as-uppercase-1 %void)
  ((idx %signed-word-integer)
   (length %signed-word-integer)
   (s2 %string))
  (if (%le idx length)
      (%let ((ch %signed-word-integer
                 (%cast %signed-word-integer (%extract s2 idx)))) ;;RR
            (if (lower? ch)
                (%setf (%extract s2 idx)
                       (%cast %unsigned-byte-integer (lower2upper
                                                      ch)))
              ())
            (as-uppercase-1 (%plus idx #%i1) length s2))
    ()))

;;;-----------------------------------------------------------------------------
;;; string-ref
;;;-----------------------------------------------------------------------------
;;(defun string-ref (string n)
;;  (convert-int-char (%extract (string-pointer string) (make-swi n))))

;;;-----------------------------------------------------------------------------
;;; (converter pair)
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; equal
;;;-----------------------------------------------------------------------------
(defmethod equal ((string1 <string>) (string2 <string>))
  (equal-string string1 string2))

;;;-----------------------------------------------------------------------------
;;; copy
;;;-----------------------------------------------------------------------------
(defmethod deep-copy ((string <string>))
  (make-string (duplicate-%string (string-pointer string))))

(defmethod shallow-copy ((string <string>))
  (make-string (duplicate-%string (string-pointer string))))

;;;-----------------------------------------------------------------------------
;;; size (in collection-string)
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; string-lt
;;;-----------------------------------------------------------------------------
;;(defmethod string-lt ((string1 <string>) (string2 <string>))
;;  (if (%lt (strcmp (string-pointer string1)
;;                   (string-pointer string2))
;;           #%i0)
;;    t
;;    ()))

;;;-----------------------------------------------------------------------------
;;; string-slice
;;;-----------------------------------------------------------------------------
;;(defun string-slice (string start end)
;;  (%let* ((start %signed-word-integer (make-swi start))
;;          (length %signed-word-integer (compute-resulting-size
;;                                        (make-swi end) start
;;                                        (strlen (string-pointer string))))
;;          (new-string-ptr %string
;;           (allocate-%string (%plus length #%i1))))
;;    (strncpy new-string-ptr
;;             (%cast %string
;;                    (%plus (%cast %signed-word-integer
;;                                          (string-pointer string))
;;                                   start))
;;             length)
;;    (%setf (%extract new-string-ptr length) #%b0)       ;terminate with 0
;;    (make-string new-string-ptr)))
;;
;;(%define-function (compute-resulting-size %signed-word-integer)
;;                  ((start %signed-word-integer)
;;                   (end %signed-word-integer)
;;                   (length %signed-word-integer))
;;   (cond ((%lt start #%i0) (compute-resulting-size #%i0 end length))
;;         ((%le end start) #%i0)
;;         ((%gt end length) (compute-resulting-size start length length))
;;         (t (%minus end start))))

;;;-----------------------------------------------------------------------------
;;; printing
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
) ;end of string
;;;-----------------------------------------------------------------------------
