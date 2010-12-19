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
;;; Title: syntactic sugar helpful in the expansion phase
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule ex-aux0
  (syntax (syntax-1)
   import (level-1))

(defmacro get-name (form)
  `(let ((x (cadr ,form)))
     (if (symbol? x)
         x
       (if (and (cons? x) (eq (car x) 'setter))
           x
         (error <condition>
                (fmt "bad value ~a" x))))))

(defmacro get-params (form) `(caddr ,form))

(defmacro get-lambda-params (form) `(cadr ,form))

(defmacro get-body (form)
  `(let ((x (cdr (cddr ,form))))
     (if (or (null? x) (cons? x))
         x
       (error <condition>
              (fmt "body ~a not a list" x)))))

(defmacro get-lambda-body (form)
  `(let ((x (cddr ,form)))
     (if (or (null? x) (cons? x))
         x
       (error <condition>
              (fmt "body ~a not a list" x)))))

(defmacro get-value (form) `(caddr ,form))

(defmacro get-directives (form) `(caddr ,form))

(defmacro get-top-level-forms (form) `(cdr (cddr ,form)))

;;;-----------------------------------------------------------------------------
)  ;; End of module ex-aux0
;;;-----------------------------------------------------------------------------
