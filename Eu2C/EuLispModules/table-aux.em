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
;;;  Title: aux file for table
;;;  Description: tables provide a general key to value association mechanism
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems: !!!hash function only for symbols, strings and fpint!!!
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------

(defmodule table-aux
  (import
   (tail
    apply
    eulisp-kernel
    (only
     (%list-size)
     basic-list)
    (only
     (symbol? <symbol>)
     symbol)
    ;;(only (prin) print)
    (only (<string> string?)
          string-ii)
    (only (eql)
          compare )
    (only (<character> convert-char-int character?)
          character)
    )
   syntax
   (tail
    syntax-0)

   export
   (mapc
    hash
    assoc
    assq
    $standard-table-mask
    $standard-table-size
    )
   )


(deflocal $standard-table-size 256)

(%define-variable $standard-table-mask %unsigned-word-integer)
(%setf $standard-table-mask
       (%minus (%cast %unsigned-word-integer
                      (make-swi
                       (%cast <int> $standard-table-size))) #%I1))


(defun mapc (function . lists)
  ;;(function &rest lists)
  (if (%eq (%list-size lists) #%i1)
      (mapc-aux1 function (car lists))
    ()
    ;;(prin 'more-than-one-list-for-mapc)
    )
  )



(defun mapc-aux1 (func lst)
  (if lst
      (progn
        (func (car lst))
        (mapc-aux1 func (cdr lst)))
    ()))


;;  (%define-function (%pair-size %signed-word-integer )
;;                    ((l <list>))
;;    (if (null? l)
;;      #%i0
;;      (%plus #%i1 (%pair-size (cdr l)))))


;;  (defun assoc (object alist . l)
;;    (%let* ((length-of-l %signed-word-integer (%list-size l))
;;            (predicate <function>
;;                       (if (%gt length-of-l #%i0)
;;                           (car l)
;;               ;;(%cast <function> (car l))
;;                           eql))
;;            (res <object> (assoc-aux object alist predicate)
;;     ;;(%cast <object>
;;     ;;       (assoc-aux object alist predicate))
;;                 ))
;;      (if res
;;        res
;;        (if (%gt length-of-l #%i1)
;;          (car (cdr l))
;;          ()))
;;      ))


;;  (defun assoc (object alist . lst)
;;    (assoc-aux object alist (if lst (car lst) eql)))

;;  (defun assoc-aux (object alist predicate)
;;         (cond ((null? alist) () )
;;               ((predicate (car (car alist)) object)
;;                (car alist))
;;               (t (assoc-aux object (cdr alist) predicate))))


(defun assoc (object alist predicate)
  (cond ((null? alist) () )
        ((predicate (car (car alist)) object)
         (car alist))
        (t (assoc object (cdr alist) predicate))))


(defun assq (object alist)
  (cond ((null? alist) ())
        ((eq (car (car alist)) object)
         (car alist))
        (t (assq object (cdr alist)))))


;;  (defun assq-aux (object alist)
;;    (cond ((null? alist) ())
;;          ((eq (car (car alist)) object)
;;           (car alist))
;;          (t (assq-aux object (cdr alist)))))


;;  (%define-function (logand <int>)
;;                    ((a <int>)
;;                     (b <int>))
;;    (make-fpint (%and (make-swi a) (make-swi b))))


;;------------------------------------------------------------
;;;hash takes an object and generates an index (for the vector)

;;  (defgeneric hash (arg))
;;
;;  (defmethod hash ((symbol <symbol>))
;;    (hash-function (symbol-name symbol)))
;;
;;  (defmethod hash ((str <string>))
;;    (hash-function str))
;;
;;  (defmethod hash ((number <number>))
;;    (logand number $standard-table-mask))


;;  (defun hash (number) ;only for numbers
;;    (print 'hash-number)
;;    (print (logand number $standard-table-mask))
;;    (logand (%cast <int> number)
;;            (%cast <int> $standard-table-mask)))

;;  (%define-function (hash %unsigned-word-integer)
;;                    ((obj <object>))
;;    (%let ((hash-num %unsigned-word-integer #%I0))
;;     (setq hash-num
;;           (cond
;;            ((symbol? obj)
;;             (hash-for-symbols obj))
;;            ((string? obj)
;; ;;(prin 'hashindex-for-strings)
;;             (hash-for-strings obj))
;;            ((character? obj)
;;             (hash-for-chars obj))
;;            (t (%cast %unsigned-word-integer (make-swi obj)))
;;            ))
;;     (%and hash-num $standard-table-mask)
;;     )
;;    )



;;;hash function only for basic data types!!!

(%define-function (hash %unsigned-word-integer)
  ((obj <object>))
  (%and (%lshiftr (%cast %unsigned-word-integer obj) #%I2)
        $standard-table-mask))


;; !!!more defmethods for the other objects!!
;;
;;;;------------------------------------------------------------



;;hash-function is the working horse
;;;;it takes a string and gives a number, normalized with
;;;;the $standard-table-mask (to cut off the bits greater than
;;;;the max length of the vector)


(%define-function (hash-for-symbols %unsigned-word-integer)
  ((sym <symbol>))
  (%let ((str %string (%select sym <symbol> name)))
        (dotimes-with-sum #%I0 str #%I0)
        ))

(%define-function (hash-for-strings %unsigned-word-integer)
  ((strng <string>))
  (%let ((str %string (%select strng <string> characters)))
        (dotimes-with-sum #%I0 str #%I0)
        ))

(%define-function (hash-for-chars %unsigned-word-integer)
  ((char <character>))
  (%let ((sum %unsigned-word-integer
              (%cast %unsigned-word-integer
                     (make-swi (convert-char-int char)))))
        sum
        ))

(%define-function (dotimes-with-sum %unsigned-word-integer)
  ((index %unsigned-word-integer)
   (str %string)
   (sum %unsigned-word-integer))
  (if (%eq (%cast %unsigned-word-integer
                  (%extract str index)) #%I0)
      sum
    (dotimes-with-sum (%plus #%I1 index)
                      str
                      (%plus
                       (%cast
                        %unsigned-word-integer
                        (%extract str index)) sum))
    ))

;;;-----------------------------------------------------------------------------
;;; type schemes for type inference
;;;-----------------------------------------------------------------------------

(%annotate-function
  assq new-signature
  (((var0 var1 var2)
    ((var var0) (atom? <null>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <null>)))
   ((var0 var1 var2)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <cons>)))))

(%annotate-function
  assoc new-signature
  (((var0 var1 var2 var3)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <object>))
    ((var var2) (atom? <list>))
    ((var var3) (atom? <object>)))))

);;;eof

