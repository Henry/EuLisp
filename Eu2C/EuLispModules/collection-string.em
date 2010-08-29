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
;;;  Title: tables provide a general key to value association mechanism
;;;  Description: collection for strings gives the functionality described in A.2
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems: setter-string-ref has to store into string.am
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------


(defmodule collection-string

  (import
   (tail
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
    (only (print)
          print)
    (only (<conversion-condition> error)
          condition)
    ;;collection-generic
    (only (strdup strlen) c-string-interface)

    collection-generic
    collection-aux
    (only (primitive-setter-string-ref
           primitive-string-length
           primitive-string-ref
           string-ref
           string-ref-u)
          collection-i)
    )

   syntax
   (tail
    syntax-0
    )

   export
   (accumulate
    accumulate1
    anyp
    concatenate
    do
    element
    emptyp
    fill
    map
    member
    reverse
    size
    ;;converter
    ;;    generic-prin
    ;;    generic-write
    )
   )

;;;------------------------------------------------------------
;;; accumulate
;;;------------------------------------------------------------


(defmethod accumulate ((function <function>)
                       (object <object>)
                       (str <string>))
  (map-accumulate function str object
                  (primitive-string-length str) #%I0))


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


;;;------------------------------------------------------------
;;; accumulate1
;;;------------------------------------------------------------


;;  (defmethod accumulate1 ((function <function>)
;;                           (str <string>))
;;     (accumulate-string1 function str))



(defmethod accumulate1 ((function <function>)
                        (str <string>))
  (if (emptyp-string str)
      ()
    (map-accumulate function str
                    (string-ref-u str #%I0)
                    (primitive-string-length str)
                    #%I1)))



;;;------------------------------------------------------------
;;; anyp
;;;------------------------------------------------------------



;;  (defmethod anyp ((function <function>)
;;                   (str <string>) . more-collections)
;;    (anyp-string function str more-collections))

(defmethod anyp ((function <function>)
                 (str <string>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (anyp-with-one-string function str #%I0
                                     (%cast %unsigned-word-integer
                                            (strlen (string-pointer str)))))
              ((%eq rest-list-length #%i1)
               (anyp-with-two-args function str (car more-collections)))
              (t (anyp-collection function str more-collections)))
        ))


(%define-function (anyp-with-one-string <object>)
  ((function <function>)
   (str <string>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer))
  (if (%lt index len)
      (if (function (string-ref-u str index))
          t
        (anyp-with-one-string function str (%plus index #%I1) len))
    nil))

(defmethod anyp-with-two-args
  ((function <function>)
   (str1 <string>)
   (str2 <string>))
  (%let*((str-length1 %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             (strlen (string-pointer str1))))
         (str-length2 %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             (strlen (string-pointer str2))))
         (min-length %unsigned-word-integer
                     (if (%le str-length1 str-length2)
                         str-length1
                       str-length2)))
        (anyp-with-two-strings function
                               str1
                               str2
                               min-length
                               #%I0)))

(%define-function (anyp-with-two-strings <object>)
  ((function <function>)
   (str1 <string>)
   (str2 <string>)
   (min-length %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index min-length)
      (if (function (string-ref-u str1 index)
                    (string-ref-u str2 index))
          t
        (anyp-with-two-strings function str1 str2 min-length
                               (%plus index #%I1)))
    nil))


(defmethod anyp-with-two-args
  ((function <function>)
   (str1 <string>)
   (collection <object>))
  (anyp-collection function str1 (cons collection nil)))



;;;anyp-string is equal to anyp-list... and can be used for all collections!!!
;;;so I take anyp-collection for all these

;;  (defun anyp-string (function liste . more-collections)
;;    (map-anyp-apply
;;     function
;;     (mapc-more-collections
;;      (cons
;;       (construct-collection-info liste)
;;       nil)
;;      more-collections)
;;     ))


;;;------------------------------------------------------------
;;; concatenate
;;;------------------------------------------------------------


(defmethod concatenate ((str <string>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (string-append str "")
               ;;             (%let ((str-length %unsigned-word-integer
               ;;                                (primitive-string-length str)))
               ;;               (concat-with-one-string str #%I0
               ;;                                       str-length
               ;;                                       (make-string
               ;;                                        str-length)))
               )
              ((%eq rest-list-length #%i1)
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
;;                     (src-length %unsigned-word-integer)
;;                     (index %unsigned-word-integer)
;;                     (start %unsigned-word-integer)
;;                     (result <string>))
;;    (if (%lt index src-length)
;;      (progn (primitive-setter-string-ref
;;              result
;;              start
;;              (primitive-string-ref src-str index))
;;             (concat-next-string src-str src-length
;;                                 (%plus index #%I1)
;;                                 (%plus start #%I1)
;;                                 result))
;;      result))

(defmethod concat-with-two-args ((str1 <string>) (str2 <string>))
  (string-append str1 str2))

(defmethod concat-with-two-args ((str1 <string>) (no-str <null>))
  (string-append str1 ""))

;;  (defmethod concat-with-two-args ((str1 <string>) (str2 <string>))
;;    (%let ((str-length1 %unsigned-word-integer
;;                         (primitive-string-length str1))
;;            (str-length2 %unsigned-word-integer
;;                         (primitive-string-length str2)))
;;           (concat-with-two-strings
;;            str1
;;            str-length1
;;            str2
;;            str-length2
;;            #%I0
;;            (make-string
;;             (%plus str-length1 str-length2)))))

;;  (%define-function (concat-with-two-strings <object>)
;;                    ((str1 <string>)
;;                     (str-length1 %unsigned-word-integer)
;;                     (str2 <string>)
;;                     (str-length2 %unsigned-word-integer)
;;                     (index %unsigned-word-integer)
;;                     (result-string <string>))
;;    (concat-with-one-string str1 index str-length1 result-string)
;;    (concat-next-string str2 str-length2 #%I0 str-length1
;;                               result-string)
;;    result-string)


(defmethod concat-with-two-args
  ((str <string>) (collection <object>))
  (concat-collection str (cons collection nil)))


;;;------------------------------------------------------------
;;; do
;;;------------------------------------------------------------

;;  (defmethod do ((function <function>)
;;                 (str <string>) . more-collections)
;;    (do-string function str more-collections))

;;;           ##
(defmethod do ((function <function>)
;;;           ##
               (str <string>) . more-collections)
  (%let ((rest-list-length %signed-word-integer
                           (%list-length more-collections)))
        (cond ((%eq rest-list-length #%i0)
               (do-with-one-string function str #%I0
                                   (%cast %unsigned-word-integer
                                          (strlen (string-pointer str)))))
              ((%eq rest-list-length #%i1)
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
    nil))

(defmethod do-with-two-args
  ((function <function>)
   (str1 <string>)
   (str2 <string>))
  (%let*((str-length1 %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             (strlen (string-pointer str1))))
         (str-length2 %unsigned-word-integer
                      (%cast %unsigned-word-integer
                             (strlen (string-pointer str2))))
         (min-length %unsigned-word-integer
                     (if (%le str-length1 str-length2)
                         str-length1
                       str-length2)))
        (do-with-two-strings function
                             str1
                             str2
                             min-length
                             #%I0)))

(%define-function (do-with-two-strings <object>)
  ((function <function>)
   (str1 <string>)
   (str2 <string>)
   (min-length %unsigned-word-integer)
   (index %unsigned-word-integer))
  (if (%lt index min-length)
      (progn (function (string-ref-u str1 index)
                       (string-ref-u str2 index))
             (do-with-two-strings function str1 str2 min-length
                                  (%plus index #%I1)))
    nil))

(defmethod do-with-two-args
  ((function <function>)
   (str1 <string>)
   (collection <object>))
  (do-collection function str1 (cons collection nil)))

;;;do-string is equal to do-list... and can be used for all collections!!!
;;;take do-collection from -aux
;;  (defun do-string (function liste . more-collections)
;;    (map-do-apply
;;     function
;;     (mapc-more-collections
;;      (cons
;;       (construct-collection-info liste)
;;       nil)
;;      more-collections)
;;     ))


;;;------------------------------------------------------------
;;; element and (setter element)
;;;------------------------------------------------------------


(defmethod element ((str <string>)
                    (key <fixed-precision-integer>))
  (string-ref str key))


(defmethod (setter element) ((str <string>)
                             (key <fixed-precision-integer>)
                             (value <character>))
  (%setf (%extract (string-pointer str)
                   (%cast %unsigned-word-integer
                          (make-swi key)))
         (%cast %unsigned-byte-integer
                (make-swi (convert-char-int value))))
  value)


;;;------------------------------------------------------------
;;; emptyp
;;;------------------------------------------------------------


(defmethod emptyp ((str <string>))
  (if  (%eq (primitive-string-ref str #%I0) #%B0)
      t
    nil))


(defun emptyp-string (str)
  (if  (%eq (primitive-string-ref str #%I0) #%B0)
      t
    nil))


;;;------------------------------------------------------------
;;; fill
;;;------------------------------------------------------------


;;  (defmethod fill ((str <string>)
;;                   (object <character>) ;object to fill
;;                   (start <fixed-precision-integer>)
;;                   (end <fixed-precision-integer>))
;;    (fill-string str object start end))


(defmethod fill ((str <string>)
                 (object <character>) . keys)
  (%let* ((str-pointer %string (string-pointer str))
          (str-len %signed-word-integer
                   (%cast %signed-word-integer
                          (strlen str-pointer)))
          (rest-list-length %signed-word-integer
                            (%list-length keys)))
         (if (%eq #%i0 rest-list-length)
             (progn
               (fill-string-aux str-pointer
                                (%cast %unsigned-byte-integer
                                       (make-swi (convert-char-int object)))
                                #%I0
                                (%cast %unsigned-word-integer
                                       (%minus str-len #%i1)))
               nil)
           (if (%eq #%i1 rest-list-length)
               (error "fill: collection does not have natural order"
                      <conversion-condition>)
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
                         nil)
                     nil))))))



(%define-function (fill-string-aux %void)
  ((str-pointer %string)
   (object %unsigned-byte-integer)
   (start %unsigned-word-integer)
   (end %unsigned-word-integer))
  (if (%gt start end)
      nil
    (progn (%setf (%extract str-pointer start)
                  object)
           (fill-string-aux str-pointer object
                            (%plus start #%I1) end))))


;;------------------------------------------------------------

;;  (defmethod map ((function <function>)
;;                  (str <string>) . more-collections)
;;    (map-string function str more-collections))

;; (defmethod map ((function <function>)
;;                 (str <string>) . more-collections)
;;   (%let ((rest-list-length %signed-word-integer
;;                            (%list-length more-collections)))
;;         (cond ((%eq rest-list-length #%i0)
;;                (%let ((str-length %unsigned-word-integer
;;                                   (%cast %unsigned-word-integer
;;                                          (strlen (string-pointer str)))))
;;                      (map-with-one-string function str #%I0
;;                                           str-length
;;                                           (make-string
;;                                            str-length))))
;;               ((%eq rest-list-length #%i1)
;;                (map-with-two-args function str (car more-collections) nil))
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
;;   (%let* ((str-length1 %unsigned-word-integer
;;                        (%cast %unsigned-word-integer
;;                               (strlen (string-pointer str1))))
;;           (str-length2 %unsigned-word-integer
;;                        (%cast %unsigned-word-integer
;;                               (strlen (string-pointer str2))))
;;           (min-length %unsigned-word-integer
;;                       (if (%le str-length1 str-length2)
;;                           str-length1
;;                         str-length2)))
;;          (map-with-two-strings function
;;                                str1
;;                                str2
;;                                min-length
;;                                #%I0
;;                                (make-uninitialized-vector
;;                                 min-length))))

;; (defmethod map-with-two-args
;;   ((function <function>)
;;    (str1 <string>)
;;    (collection <object>)
;;    (not-used <object>))
;;   (map-collection function str1 (cons collection nil)))

;;;map-string is equal to map-list... and can be used for all collections!!!
;;;take map-collection from -aux
;;  (defun map-string (function liste . more-collections) ; ; ; ; ; ; ; ;
;;    (map-tab-apply      ;; ;           ; ; ; ; ; ;
;;     function           ;; ;           ; ; ; ; ; ;
;;     (mapc-more-collections            ; ; ; ; ; ; ; ;
;;      (cons             ;; ;           ; ; ; ; ; ;
;;       (construct-collection-info liste) ; ; ; ; ; ; ; ;
;;       nil)             ;; ;           ; ; ; ; ; ;
;;      more-collections) ;; ;           ; ; ; ; ; ;
;;     nil                ;; ;           ; ; ; ; ; ;
;;     liste))            ;; ;           ; ; ; ; ; ;


;;;------------------------------------------------------------
;;; member
;;;------------------------------------------------------------


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
    nil))


(%define-function (memq-string <object>)
  ((object <character>)
   (str <string>)
   (index %unsigned-word-integer)
   (len %unsigned-word-integer))
  (if (%lt index len)
      (if (eq (string-ref-u str index) object)
          t
        (memq-string object str (%plus index #%I1) len))
    nil))




;;;------------------------------------------------------------
;;; reverse
;;;------------------------------------------------------------


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
;;           (print rev-str)
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
    rev-str)
  )




;;;------------------------------------------------------------
;;; size
;;;------------------------------------------------------------


;;  (defmethod size ((str <string>))
;;    (string-length str))


(defmethod size ((str <string>))
  (make-fpint (strlen (string-pointer str))))

;;  (defun size-string (str)
;;    (make-fpint (strlen (string-pointer str))))


)

;;;eof
