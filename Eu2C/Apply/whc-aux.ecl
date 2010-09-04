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
;;;  Title: auxiliary functions
;;;  Description:
;;    the functions for generation of code
;;;  Documentation:
;;;  Notes:
;;;  Authors: Winfried Heicking
;;;-----------------------------------------------------------------------------

;;; begin whc-aux.em

#module whc-aux

(import
 (level-1
  expand-literal

  lzs
  mzs
  simple-programming

  accessors
  representation ; whc-classes

  (only ( subtypep type-of) common-lisp)
  )

 syntax
 (level-1
  apply-standard
  lzs-modules)

 export
 (give-basic-data-type
  give-pointer-data-type
  give-prestruct-data-type
  give-union-data-type
  give-variable
  give-constant
  give-variable-or-constant
  basic-data-type?
  data-float?
  data-integer?
  )
 )



;;;-------------------------------------------------------------

(defmethod data-type? ((type <class-def>))
  t)

(defmethod data-type? ((type <number>))
  ())

(defmethod data-type? (type)
  ())

(defun variable? (var)
  (subtypep (type-of var) <var>))

(defun constant? (var)
  (subtypep (type-of var) <named-const>))

(defun pointer-data-type? (type)
  (subtypep (type-of type) <%pointer>))

(defun prestruct-data-type? (type)
  (subtypep (type-of type) <%prestruct>))

(defun union-data-type? (type)
  (subtypep (type-of type) <%union>))

(defmethod basic-data-type? ((type <basic-class-def>))
  t)


(defmethod basic-data-type? (type)
  ())

(defun give-data-type (symbol)
  (when (data-type? symbol)
        symbol))


(defun give-basic-data-type (symbol)
  (when (basic-data-type? symbol)
        symbol))


(defun give-pointer-data-type (symbol)
  (when (pointer-data-type? symbol)
        symbol))

(defun give-prestruct-data-type (symbol)
  (when (prestruct-data-type? symbol)
        symbol))

(defun give-union-data-type (symbol)
  (when (union-data-type? symbol)
        symbol))

(defun give-variable (var)
  (when (variable? var)
        var))

(defun give-constant (var)
  (when (constant? var)
        var))

(defun give-variable-or-constant (var)
  (when (or (constant? var)
            (variable? var))
        var))

(defmethod data-integer? ((instance <%integer>))
  t)

(defmethod data-integer? (object)
  ())

(defmethod data-float? ((instance <%float>))
  t)

(defmethod data-float? (object)
  ())

#module-end
;;; eof whc-aux.lisp
