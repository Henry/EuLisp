;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp kernel syntax
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule syntax
  (syntax (macros)
   import (root)
   export (defmacro
           quasiquote
           unquote
           unquote-splicing
           macroexpand1
           macroexpand
           %defun
           letfuns))

;; (define (letfun-binding binding)
;;         (list
;;          (car binding)
;;          (cons 'lambda (cdr binding))))

(%defun letfun-binding (binding)
        (list
         (car binding)
         (cons 'lambda (cdr binding))))

(defmacro letfuns (funs . body)
  `(letrec
    ,(map-list letfun-binding funs)
    ,@body))

;;;-----------------------------------------------------------------------------
)  ;; End of module syntax
;;;-----------------------------------------------------------------------------
