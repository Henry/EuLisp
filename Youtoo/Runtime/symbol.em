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
;;; Title: symbols
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule symbol
  (syntax (_telos0)
   import (telos
           convert
           compare
           collect
           character
           string
           table
           fpi)
   export (<symbol>
           <keyword>
           <name>
           symbol?
           keyword?
           name
           gensym
           symbol-name
           keyword-name
           symbol-exists?))

;;;-----------------------------------------------------------------------------
;;; Symbol and keyword initialization
;;;-----------------------------------------------------------------------------
(defmethod initialize ((sym <symbol>) inits)
  (call-next-method)
  (eul_init_symbol sym))

(defmethod initialize ((key <keyword>) inits)
  (call-next-method)
  (eul_init_keyword key))

(defextern eul_init_symbol (ptr) ptr)
(defextern eul_init_keyword (ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Gensym
;;;-----------------------------------------------------------------------------
(deflocal *symbol-count* 0)

(defun gensym strs
  (let* ((prestr (if strs (car strs) "G00"))
         (sym (make-symbol
               (string-append prestr (fpi-as-string *symbol-count*)))))
    (setq *symbol-count* (fpi-binary+ *symbol-count* 1))
    sym))

;;;-----------------------------------------------------------------------------
;;; More predicates ...
;;;-----------------------------------------------------------------------------
(defun keyword? (x)
  (eq (class-of x) <keyword>))

(defun symbol-exists? (str)
  (table-ref (get-global-register symbols) str))

;; Removing this apparently unused definition causes Youtoo to seg-fault
(defgeneric namep (x)
  method: ((x))
  method: (((x <name>)) t))

;;;-----------------------------------------------------------------------------
;;; Comparison
;;;-----------------------------------------------------------------------------
(defmethod binary< ((sym1 <symbol>) (sym2 <symbol>))
  (fpi-binary< (string-compare (symbol-name sym1)
                               (symbol-name sym2)) 0))

;;;-----------------------------------------------------------------------------
;;; Concatenation
;;;-----------------------------------------------------------------------------
(defmethod concatenate ((x <name>) . cs)
  (labels
   ((loop (ccs str)
          (if (null? ccs)
              (make (class-of x) name: str)
            (loop (cdr ccs)
                  (string-append str (convert (car ccs) <string>))))))
   (loop cs (name x))))

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
(defgeneric (converter <symbol>) (x))

(defmethod (converter <symbol>) ((str <string>))
  (make <symbol> name: str))

;;;-----------------------------------------------------------------------------
)  ;; End of module symbol
;;;-----------------------------------------------------------------------------
