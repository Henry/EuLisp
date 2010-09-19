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
;;; Title: tables provide a general key to value association mechanism
;;;  Description: collection for strings gives the functionality described in A.2
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems: setter-string-ref has to store into string.am
;;;  Authors: Winfried Heicking
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule collection-string
  (import (tail
           apply
           eulisp-kernel
           ;;collection-aux
           (only (<string>
                  string-pointer
                  make-string
                  string-append)
                 string-ii)
           basic-list
           character
           (only (eql) compare)
           (only (print
                  nl)
                 print)
           (only (<conversion-condition>
                  error)
                 condition)
           ;;collection-generic
           (only (strdup
                  strlen)
                 c-string-interface)
           collection-generic
           collection-aux
           (only (primitive-setter-string-ref
                  primitive-string-size
                  primitive-string-ref
                  string-ref
                  string-ref-u)
                 collection-i))
   syntax (tail
           syntax-0)
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
           reverse
           size
           ;;converter
           ;;    generic-print
           ;;    generic-write
           ))

;;;-----------------------------------------------------------------------------
;;; accumulate
;;;-----------------------------------------------------------------------------
(defmethod accumulate ((function <function>)
                       (object <object>)
                       (str <string>))
  (map-accumulate function str object
                  (primitive-string-size str) #%I0))

(%define-function (map-accumulate <object>)
  ((function <function>)
   (str <string>)
   (res <object>)
   (max-len %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index max-len)
      (map-accumulate function
                      str
                      (function res (string-ref-u str index))
                      max-len
                      (%plus index #%I1))
    res))

;;  (defun map-accumulate (function str res max-len index)
;;    (if (< index max-len)
;;      (map-accumulate function
;;                      str
;;                      (function res (string-ref str index));!!!take this if funcall ok
;;                      max-len
;;                      (+ index 1))
;;      res))

;;;-----------------------------------------------------------------------------
;;; accumulate1
;;;-----------------------------------------------------------------------------
;;  (defmethod accumulate1 ((function <function>)
;;                           (str <string>))
;;     (accumulate-string1 function str))

(defmethod accumulate1 ((function <function>)
                        (str <string>))
  (if (empty?-string str)
      ()
    (map-accumulate function str
                    (string-ref-u str #%I0)
                    (primitive-string-size str)
                    #%I1)))

;;;-----------------------------------------------------------------------------
;;; any?
;;;-----------------------------------------------------------------------------
;;  (defmethod any? ((function <function>)
;;                   (str <string>) . more-collections)
;;    (any?-string function str more-collections))

(defmethod any? ((function <function>)
                 (str <string>) . more-collections)
  (%let ((rest-list-size %signed-word-integer
                           (%list-size more-collections)))
        (cond ((%eq rest-list-size #%i0)
               (any?-with-one-string function str #%I0
                                     (%cast %unsigned-word-integer
                                            (strlen (string-pointer str)))))
              ((%eq rest-list-size #%i1)
               (any?-with-two-args function str (car more-collections)))
              (t (any?-collection function str more-collections)))
        ))

(%define-function (any?-with-one-string <object>)
  ((function <function>)
   (str <string>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer))
  (if (%lt index len)
      (if (function (string-ref-u str index))
          t
        (any?-with-one-string function str (%plus index #%I1) len))
    ()))

(defmethod any?-with-two-args
  ((function <function>)
   (str1 <string>)
   (str2 <string>))
  (%let*((str-size1 %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             (strlen (string-pointer str1))))
         (str-size2 %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             (strlen (string-pointer str2))))
         (min-size %unsigned-word-integer
                     (if (%le str-size1 str-size2)
                         str-size1
                       str-size2)))
        (any?-with-two-strings function
                               str1
                               str2
                               min-size
                               #%I0)))

(%define-function (any?-with-two-strings <object>)
  ((function <function>)
   (str1 <string>)
   (str2 <string>)
   (min-size %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index min-size)
      (if (function (string-ref-u str1 index)
                    (string-ref-u str2 index))
          t
        (any?-with-two-strings function str1 str2 min-size
                               (%plus index #%I1)))
    ()))

(defmethod any?-with-two-args
  ((function <function>)
   (str1 <string>)
   (collection <object>))
  (any?-collection function str1 (cons collection ())))

;;;any?-string is equal to any?-list... and can be used for all collections!!!
;;;so I take any?-collection for all these

;;  (defun any?-string (function liste . more-collections)
;;    (map-any?-apply
;;     function
;;     (mapc-more-collections
;;      (cons
;;       (construct-collection-info liste)
;;       ())
;;      more-collections)
;;     ))

;;;-----------------------------------------------------------------------------
;;; concatenate
;;;-----------------------------------------------------------------------------
(defmethod concatenate ((str <string>) . more-collections)
  (%let ((rest-list-size %signed-word-integer
                           (%list-size more-collections)))
        (cond ((%eq rest-list-size #%i0)
               (string-append str "")
               ;;             (%let ((str-size %unsigned-word-integer
               ;;                                (primitive-string-size str)))
               ;;               (concat-with-one-string str #%I0
               ;;                                       str-size
               ;;                                       (make-string
               ;;                                        str-size)))
               )
              ((%eq rest-list-size #%i1)
               (concat-with-two-args str (car more-collections)))
              (t (concat-collection str more-collections)))
        ))

;;  (%define-function (concat-with-one-string <string>)
;;                     ((str <string>)
;;                     (index %unsigned-word-integer)
;;                     (len %unsigned-word-integer)
;;                     (result <string>))
;;    (if (%lt index len)
;;      (progn (primitive-setter-string-ref
;;               result
;;               index
;;               (primitive-string-ref str index))
;;             (concat-with-one-string str
;;                                  (%plus index #%I1) len result))
;;      result)
;;    )

;;  (%define-function (concat-next-string <string>)
;;                    ((src-str <string>)
;;                     (src-size %unsigned-word-integer)
;;                     (index %unsigned-word-integer)
;;                     (start %unsigned-word-integer)
;;                     (result <string>))
;;    (if (%lt index src-size)
;;      (progn (primitive-setter-string-ref
;;              result
;;              start
;;              (primitive-string-ref src-str index))
;;             (concat-next-string src-str src-size
;;                                 (%plus index #%I1)
;;                                 (%plus start #%I1)
;;                                 result))
;;      result))

(defmethod concat-with-two-args ((str1 <string>) (str2 <string>))
  (string-append str1 str2))

(defmethod concat-with-two-args ((str1 <string>) (no-str <null>))
  (string-append str1 ""))

;;  (defmethod concat-with-two-args ((str1 <string>) (str2 <string>))
;;    (%let ((str-size1 %unsigned-word-integer
;;                         (primitive-string-size str1))
;;            (str-size2 %unsigned-word-integer
;;                         (primitive-string-size str2)))
;;           (concat-with-two-strings
;;            str1
;;            str-size1
;;            str2
;;            str-size2
;;            #%I0
;;            (make-string
;;             (%plus str-size1 str-size2)))))

;;  (%define-function (concat-with-two-strings <object>)
;;                    ((str1 <string>)
;;                     (str-size1 %unsigned-word-integer)
;;                     (str2 <string>)
;;                     (str-size2 %unsigned-word-integer)
;;                     (index %unsigned-word-integer)
;;                     (result-string <string>))
;;    (concat-with-one-string str1 index str-size1 result-string)
;;    (concat-next-string str2 str-size2 #%I0 str-size1
;;                               result-string)
;;    result-string)

(defmethod concat-with-two-args
  ((str <string>) (collection <object>))
  (concat-collection str (cons collection ())))

;;;-----------------------------------------------------------------------------
;;; do
;;;-----------------------------------------------------------------------------

;;  (defmethod do ((function <function>)
;;                 (str <string>) . more-collections)
;;    (do-string function str more-collections))

;;;           ##
(defmethod do ((function <function>)
;;;           ##
               (str <string>) . more-collections)
  (%let ((rest-list-size %signed-word-integer
                           (%list-size more-collections)))
        (cond ((%eq rest-list-size #%i0)
               (do-with-one-string function str #%I0
                                   (%cast %unsigned-word-integer
                                          (strlen (string-pointer str)))))
              ((%eq rest-list-size #%i1)
               (do-with-two-args function str (car more-collections)))
              (t (do-collection function str more-collections)))
        ))

(%define-function (do-with-one-string <object>)
  ((function <function>)
   (str <string>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer))
  (if (%lt index len)
      (progn (function (string-ref-u str index))
             (do-with-one-string function str (%plus index #%I1) len))
    ()))

(defmethod do-with-two-args
  ((function <function>)
   (str1 <string>)
   (str2 <string>))
  (%let*((str-size1 %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             (strlen (string-pointer str1))))
         (str-size2 %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             (strlen (string-pointer str2))))
         (min-size %unsigned-word-integer
                     (if (%le str-size1 str-size2)
                         str-size1
                       str-size2)))
        (do-with-two-strings function
                             str1
                             str2
                             min-size
                             #%I0)))

(%define-function (do-with-two-strings <object>)
  ((function <function>)
   (str1 <string>)
   (str2 <string>)
   (min-size %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index min-size)
      (progn (function (string-ref-u str1 index)
                       (string-ref-u str2 index))
             (do-with-two-strings function str1 str2 min-size
                                  (%plus index #%I1)))
    ()))

(defmethod do-with-two-args
  ((function <function>)
   (str1 <string>)
   (collection <object>))
  (do-collection function str1 (cons collection ())))

;;;do-string is equal to do-list... and can be used for all collections!!!
;;;take do-collection from -aux
;;  (defun do-string (function liste . more-collections)
;;    (map-do-apply
;;     function
;;     (mapc-more-collections
;;      (cons
;;       (construct-collection-info liste)
;;       ())
;;      more-collections)
;;     ))

;;;-----------------------------------------------------------------------------
;;; element and (setter element)
;;;-----------------------------------------------------------------------------
(defmethod element ((str <string>)
                    (key <int>))
  (string-ref str key))

(defmethod (setter element) ((str <string>)
                             (key <int>)
                             (value <character>))
  (%setf (%extract (string-pointer str)
                   (%cast %unsigned-word-integer
                          (make-swi key)))
         (%cast %unsigned-byte-integer
                (make-swi (convert-char-int value))))
  value)

;;;-----------------------------------------------------------------------------
;;; empty?
;;;-----------------------------------------------------------------------------
(defmethod empty? ((str <string>))
  (if  (%eq (primitive-string-ref str #%I0) #%B0)
      t
    ()))

(defun empty?-string (str)
  (if  (%eq (primitive-string-ref str #%I0) #%B0)
      t
    ()))

;;;-----------------------------------------------------------------------------
;;; fill
;;;-----------------------------------------------------------------------------
;;  (defmethod fill ((str <string>)
;;                   (object <character>) ;object to fill
;;                   (start <int>)
;;                   (end <int>))
;;    (fill-string str object start end))

(defmethod fill ((str <string>)
                 (object <character>) . keys)
  (%let* ((str-pointer %string (string-pointer str))
          (str-len %signed-word-integer
                   (%cast %signed-word-integer
                          (strlen str-pointer)))
          (rest-list-size %signed-word-integer
                            (%list-size keys)))
         (if (%eq #%i0 rest-list-size)
             (progn
               (fill-string-aux str-pointer
                                (%cast %unsigned-byte-integer
                                       (make-swi (convert-char-int object)))
                                #%I0
                                (%cast %unsigned-word-integer
                                       (%minus str-len #%i1)))
               ())
           (if (%eq #%i1 rest-list-size)
               (error <conversion-condition>
                      "fill: collection does not have natural order")
             (%let ((start %signed-word-integer
                           (make-swi (car keys)))
                    (end %signed-word-integer
                         (make-swi (car (cdr keys)))))
                   (if (test-range-indizes start end str-len)
                       (progn
                         (fill-string-aux
                          str-pointer
                          (%cast %unsigned-byte-integer
                                 (make-swi (convert-char-int object)))
                          (%cast %unsigned-word-integer start)
                          (%cast %unsigned-word-integer end))
                         ())
                     ()))))))

(%define-function (fill-string-aux %void)
  ((str-pointer %string)
   (object %unsigned-byte-integer)
   (start %unsigned-word-integer)
   (end %unsigned-word-integer))
  (if (%gt start end)
      ()
    (progn (%setf (%extract str-pointer start)
                  object)
           (fill-string-aux str-pointer object
                            (%plus start #%I1) end))))

;;-----------------------------------------------------------------------------

;;  (defmethod map ((function <function>)
;;                  (str <string>) . more-collections)
;;    (map-string function str more-collections))

;; (defmethod map ((function <function>)
;;                 (str <string>) . more-collections)
;;   (%let ((rest-list-size %signed-word-integer
;;                            (%list-size more-collections)))
;;         (cond ((%eq rest-list-size #%i0)
;;                (%let ((str-size %unsigned-word-integer
;;                                   (%cast %unsigned-word-integer
;;                                          (strlen (string-pointer str)))))
;;                      (map-with-one-string function str #%I0
;;                                           str-size
;;                                           (make-string
;;                                            str-size))))
;;               ((%eq rest-list-size #%i1)
;;                (map-with-two-args function str (car more-collections) ()))
;;               (t (map-collection function str more-collections)))
;;         ))

;; (%define-function (map-with-one-string <string>)
;;                   ((function <function>)
;;                    (str <string>)
;;                    (index %unsigned-word-integer)
;;                    (len %unsigned-word-integer)
;;                    (result <string>))
;;                   (if (%lt index len)
;;                       (progn (setf-primitive-vector-ref
;;                               result
;;                               index
;;                               (function (primitive-vector-ref str index)))
;;                              (map-with-one-string function str
;;                                                   (%plus index #%I1) len result))
;;                     result))

;; (defmethod map-with-two-args
;;   ((function <function>)
;;    (str1 <string>)
;;    (str2 <string>)
;;    (not-used <object>))
;;   (%let* ((str-size1 %unsigned-word-integer
;;                        (%cast %unsigned-word-integer
;;                               (strlen (string-pointer str1))))
;;           (str-size2 %unsigned-word-integer
;;                        (%cast %unsigned-word-integer
;;                               (strlen (string-pointer str2))))
;;           (min-size %unsigned-word-integer
;;                       (if (%le str-size1 str-size2)
;;                           str-size1
;;                         str-size2)))
;;          (map-with-two-strings function
;;                                str1
;;                                str2
;;                                min-size
;;                                #%I0
;;                                (make-uninitialized-vector
;;                                 min-size))))

;; (defmethod map-with-two-args
;;   ((function <function>)
;;    (str1 <string>)
;;    (collection <object>)
;;    (not-used <object>))
;;   (map-collection function str1 (cons collection ())))

;;;map-string is equal to map-list... and can be used for all collections!!!
;;;take map-collection from -aux
;;  (defun map-string (function liste . more-collections) ; ; ; ; ; ; ; ;
;;    (map-tab-apply      ;; ;           ; ; ; ; ; ;
;;     function           ;; ;           ; ; ; ; ; ;
;;     (mapc-more-collections            ; ; ; ; ; ; ; ;
;;      (cons             ;; ;           ; ; ; ; ; ;
;;       (construct-collection-info liste) ; ; ; ; ; ; ; ;
;;       ())             ;; ;           ; ; ; ; ; ;
;;      more-collections) ;; ;           ; ; ; ; ; ;
;;     ()                 ;; ;           ; ; ; ; ; ;
;;     liste))            ;; ;           ; ; ; ; ; ;

;;;-----------------------------------------------------------------------------
;;; member
;;;-----------------------------------------------------------------------------
;;  (defmethod member ((object <character>)
;;                     (str <string>) . test)
;;    (member-string object str test))

(defmethod member ((char <character>)
                   (str <string>) . test)
  (let ((test-fct (if test
                      (car test)
                    eql)))
    (if (eq test-fct eq)
        (memq-string char str #%I0
                     (%cast %unsigned-word-integer
                            (strlen (string-pointer str))))
      (member-string-aux char str #%I0
                         (%cast
                          %unsigned-word-integer
                          (strlen (string-pointer str)))
                         test-fct))))

(%define-function (member-string-aux <object>)
  ((object <character>)
   (str <string>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer)
   (test <function>))
  (if (%lt index len)
      (if (test (string-ref-u str index) object)
          t
        (member-string-aux object str (%plus index #%I1) len test))
    ()))

(%define-function (memq-string <object>)
  ((object <character>)
   (str <string>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer))
  (if (%lt index len)
      (if (eq (string-ref-u str index) object)
          t
        (memq-string object str (%plus index #%I1) len))
    ()))

;;;-----------------------------------------------------------------------------
;;; reverse
;;;-----------------------------------------------------------------------------
(defmethod reverse ((str <string>))
  (%let* ((str-pointer %string
                       (string-pointer str))
          (str-len %unsigned-word-integer
                   (%cast %unsigned-word-integer
                          (strlen str-pointer)))
          (rev-str <string>
                   (make-string (strdup str-pointer))))
         (if (%eq str-len #%I0)
             rev-str
           (reverse-string-aux str rev-str #%I0
                               (%minus str-len #%I1)))))

;;  (defun reverse-string (str)
;;    (%let* ((str-pointer %string
;;                         (string-pointer str))
;;            (str-len %unsigned-word-integer
;;                     (%cast %unsigned-word-integer
;;                            (strlen str-pointer)))
;;            (rev-str <string>
;;                     (make-string (strdup str-pointer))))
;;           (print rev-str nl)
;;           (if (%eq str-len #%I0)
;;             rev-str
;;             (reverse-string-aux str rev-str #%I0
;;                                 (%minus str-len #%I1)))))

(%define-function (reverse-string-aux <string>)
  ((str <string>)
   (rev-str <string>)
   (index %unsigned-word-integer)
   (str-len %unsigned-word-integer))
  (if (%le index str-len)
      (progn
        (primitive-setter-string-ref
         rev-str
         index
         (primitive-string-ref str (%minus str-len index)))
        (reverse-string-aux str rev-str (%plus index #%I1) str-len))
    rev-str))

;;;-----------------------------------------------------------------------------
;;; size
;;;-----------------------------------------------------------------------------
(defmethod size ((str <string>))
  (make-fpint (strlen (string-pointer str))))

;;;-----------------------------------------------------------------------------
)
;;;-----------------------------------------------------------------------------
