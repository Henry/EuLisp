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
;;; Title: EuLisp Level-0 setter functionality
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule setter
  (syntax (macros)
   import (root)
   export (setter
           setter-setter))

(deflocal setter-table (make-table))

(define (setter obj)
        (table-ref setter-table obj))

;;   (defmacro setter (obj)
;;     `(table-ref setter-table ,obj))

(define (setter-setter obj val)
        (table-set! setter-table obj val))

(setter-setter setter setter-setter)

(setter-setter string-ref string-set!)
(setter-setter vector-ref vector-set!)
(setter-setter get-file-position set-file-position!)
(setter-setter symbol-value set-symbol-value!)
(setter-setter symbol-plist set-symbol-plist!)
(setter-setter table-ref table-set!)
(setter-setter table-fill set-table-fill!)

;;;-----------------------------------------------------------------------------
)  ;; End of module setter
;;;-----------------------------------------------------------------------------
