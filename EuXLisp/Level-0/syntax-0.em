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
;;; Title: EuLisp Level-0 syntax-0 module
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule syntax-0
  (syntax (syntax)
   import (root
           thread
           condition)
   export (;; Imported from macros via syntax
           defmacro
           quasiquote
           unquote
           unquote-splicing

           ;; Imported from macros
           defun
           defgeneric
           defmethod
           generic-lambda
           method-lambda
           defmodule
           import
           syntax
           letfuns

           ;; Defined in this module
           block
           return-from
           when
           unless
           while))

(defmacro block (tag . body)
  (if (symbol? tag)
      `(let/cc ,tag ,@body)
    (error <compilation-general-error>
           "not a symbol in block"
           value: tag)))

(defmacro return-from (tag . val)
  (if (symbol? tag)
      (if (null? val)
          `(,tag ())
        `(,tag ,@val))
    (error <compilation-general-error>
           "not a symbol in return-from"
           value: tag)))

(defmacro when (test . body)
  `(if ,test (progn ,@body) ()))

(defmacro unless (test . body)
  `(if ,test () (progn ,@body)))

(defmacro while (test . body)
  `(let/cc {break}                    ; break can be captured in body
     (letrec
      ((loop (lambda ()
               (when ,test
                     ,@body
                     (loop)))))
      (loop))))

;;;-----------------------------------------------------------------------------
)  ;; End of module syntax-0
;;;-----------------------------------------------------------------------------
