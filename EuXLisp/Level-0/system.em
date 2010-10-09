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
   export (%MAP1
           %FOR-EACH1
           %WITH-FILE1
           %LOAD1
           %FORCE1
           %INITLOOP1
           %CAR
           %CDR
           %SET-CAR!
           %SET-CDR!
           %VECTOR-LENGTH
           %VECTOR-REF
           %VECTOR-SET!
           %KEYWORDS
           %MAKE-CONSTANT
           %IMPORT

           *INTIALIZE*
           *TOPLEVEL*
           **EOF**
           **DEFAULT**
           *UNBOUND*
           stdin
           stdout
           stderr
           *FIXNUM-FORMAT*
           *FLONUM-FORMAT*
           *PRINT-CASE*
           *gc-msgs*

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
        (%VECTOR-REF (%CAR closure) 0))

(define (getliteral closure n)
        (%VECTOR-REF (%CAR closure) n))

;;;-----------------------------------------------------------------------------
)  ;; End of module system
;;;-----------------------------------------------------------------------------
