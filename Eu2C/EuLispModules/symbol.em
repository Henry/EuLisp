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
;;;  Title: standard module symbol (eulisp level 0)
;;;  Description: some more thingies for symbols
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;    generation of gensym-name is a hack use converter number-string from printer if
;;    possible
;;;  Authors: Ingo Mohr, E. Ulrich Kriegel
;;;-----------------------------------------------------------------------------

(defmodule symbol
  (import
   (tail
    basic-list
    basic-symbol
    c-string-interface
    ;; printer-generic
    string-ii
    (only (find-option) option-lists)
    (only (initialize) object-0-i)
    (only (sprintf-3) c-stdio))
   syntax (tail syntax-0)
   export
   (<symbol>
    symbolp
    gensym
    gensym1
    symbol-name
    symbol-exists-p
    make-symbol
    initialize))

;;;-----------------------------------------------------------------------------
;;; <symbol> and symbolp are defined in basic-symbol
;;; Also the literal-expansion for symbols is defined there.
;;;-----------------------------------------------------------------------------
(defgeneric (converter <symbol>)(object))

;;;-----------------------------------------------------------------------------
;;; make-symbol
;;;-----------------------------------------------------------------------------
(%define-function (make-symbol <symbol>) ((name <string>))
  (or (find-symbol-in-table name *symbol-table*)
      (let ((symbol (make-symbol-without-copy
                     (duplicate-%string
                      (%select name <string> characters)))))
        (add-symbol symbol)
        symbol)))

;;;-----------------------------------------------------------------------------
;;; initialize
;;;-----------------------------------------------------------------------------
(defmethod initialize ((object <symbol>) . initlist)
  (let ((option (find-option 'string initlist ())))
    (if option
        (make-symbol (car option))
      (make-symbol ""))))

;;;-----------------------------------------------------------------------------
;;; symbol-name
;;;-----------------------------------------------------------------------------
(%define-function (symbol-name <string>) ((symbol <symbol>))
  (make-string (duplicate-%string (%select symbol <symbol> name))))

;;;-----------------------------------------------------------------------------
;;; gensym
;;;-----------------------------------------------------------------------------
(%define-variable $gensym-counter %unsigned-word-integer)
(%setf $gensym-counter #%I0)
(%define-variable  $converter-string %string)
(%setf $converter-string (allocate-%string #%i20))

(%define-function (convert-to-%string %string)
  ((val %unsigned-word-integer))
  (sprintf-3 $converter-string
             (%literal %string 2 "%d")
             (%cast %signed-word-integer val))
  $converter-string)

(defun gensym string
  (if (null? string)
      (gensym1 "G")
    (gensym1 (car string))))

(defun gensym1(arg)
  (let ((new-name
         (string-append arg
                        (make-string (convert-to-%string $gensym-counter)))))
    (%setf $gensym-counter (%plus $gensym-counter #%I1))
    (if (symbol-exists-p new-name)
        (gensym1 arg)
      (make-symbol new-name)))
  )

;;;-----------------------------------------------------------------------------
;;; symbol-exists-p
;;;-----------------------------------------------------------------------------
(defun symbol-exists-p (name)
  (find-symbol-in-table name *symbol-table*))

(%define-function (find-symbol-in-table <object>)
  ((name <string>)
   (table <list>))
  (cond ((null? table)
         ())
        ((%eq #%i0
              (strcmp (%select name <string> characters)
                      (%select (car table) <symbol> name)))
         (car table))
        (t (find-symbol-in-table name (cdr table)))))

;;;-----------------------------------------------------------------------------
;;; printing
;;;-----------------------------------------------------------------------------
;;   (defmethod generic-write ((symbol <symbol>)
;;                             (stream <stream>))
;;     (write-string-to-stream (%select symbol <symbol> name) stream))

;;;-----------------------------------------------------------------------------
;;; Type schemes for type inference
;;;-----------------------------------------------------------------------------

;; symbolp is generated automatically

(%annotate-function
  gensym new-signature
  (((var0 var1)
    ((var var0) (atom? <symbol>))
    ((var var1) (atom? <list>)))))

(%annotate-function
  gensym1 new-signature
  (((var0 var1)
    ((var var0) (atom? <symbol>))
    ((var var1) (atom? <string>)))))

(%annotate-function
  symbol-exists-p new-signature
  (((var0 var1)
    ((var var0) (atom? <object>))
    ((var var1) (atom? <string>)))))

;;;-----------------------------------------------------------------------------
) ;; end of symbol
;;;-----------------------------------------------------------------------------
