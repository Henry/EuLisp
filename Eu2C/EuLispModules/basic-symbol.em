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
;;;  Title: 
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

(defmodule basic-symbol

  (import (apply-level-2)

   syntax (apply-level-2)

   export (<symbol>
           symbolp
           make-symbol-without-copy
           #+(:symbol-with-plist :t)  symbol-plist
           *symbol-table*
           add-symbol
           ))

;;;-----------------------------------------------------------------------------
;;; <symbol> and symbolp
;;;-----------------------------------------------------------------------------
(%define-standard-class (<symbol> <class>)
  <object>
  ((name type %string
         keyword name)
   #+(:symbol-with-plist :t)
   (plist type <list> keyword plist default ()
          accessor symbol-plist ))
  representation pointer-to-struct
  allocation multiple-type-card
  constructor (make-symbol-without-copy name)
  predicate symbolp)



#-(:symbol-with-plist :t)
(%define-literal-expansion symbol
  `(%literal ,<symbol> name (%literal ,%string () ,name) )
  )
#+(:symbol-with-plist :t)
(%define-literal-expansion symbol
  `(%literal ,<symbol> name (%literal ,%string () ,name) plist ())
  )

;;;-----------------------------------------------------------------------------
;;; the symbol table
;;;-----------------------------------------------------------------------------

(deflocal *symbol-table* nil)

(%define-variable symbol-table-initialization-function %function)
;; this variable is set by the main initialization function to a function calling
;; add-symbol for every static symbol

(%funcall symbol-table-initialization-function)
;; after this call the symbol table is filled with the static symbols and may be
;; used by functions creating symbols dynamically

(defun add-symbol (sym)
  (setq *symbol-table* (cons sym *symbol-table*)))

(%annotate-function add-symbol
  is-special-function add-symbol)
(%annotate-binding symbol-table-initialization-function
  is-special-binding symtab-initfun-var)

) ;end of basic-symbol.am



