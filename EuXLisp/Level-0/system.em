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
;;; Title: Additional access to system internals
;;;  Description:
;;    The exported symbols are those not exported from level-0
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule system
  (import (root)
   export (%map1
           %for-each1
           %with-file1
           %load1
           %force1
           %initloop1
           %car
           %cdr
           %set-car!
           %set-cdr!
           %vector-size
           %vector-ref
           %vector-set!
           %keywords
           %make-constant
           %import

           *intialize*
           *toplevel*
           **eof**
           **default**
           *unbound*
           *fixnum-format*
           *flonum-format*
           *print-case*

           *gc-msgs*

           ;; Standard streams
           stdin
           stdout
           stderr

           ;; Setters
           set-car!
           set-cdr!
           vector-set!
           set-file-position!
           set-symbol-value!
           set-symbol-plist!
           table-set!
           set-table-fill!
           string-set!

           set-module
           find-module
           reintern-syntax
           get-syntax
           put-syntax
           qualified-symbols?

           define-generic
           define-method

           getbcode
           getliteral
           setivar
           getivar
           tmpfile

           the-environment
           procedure-environment
           environment?
           environment-bindings
           environment-parent

           check-ref))

(define (getbcode closure)
        (%vector-ref (%car closure) 0))

(define (getliteral closure n)
        (%vector-ref (%car closure) n))

;;;-----------------------------------------------------------------------------
)  ;; End of module system
;;;-----------------------------------------------------------------------------
