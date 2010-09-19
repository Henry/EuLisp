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
;;; Title: interface module apply
;;;  Description:
;;;  Documentation:
;;;  Notes:
;;;  Requires:
;;;  Problems:
;;;  Authors: Horst Friedrich
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------


(defmodule apply

  (import (apply-level-2
           function
           letcc
           )
   syntax (apply-level-2)
   expose (apply-level-2
           function
           basic-symbol   ; provides symbols with literal expansion
           letcc
           (rename ((%unsigned-byte-integer %character))
                   (only (%unsigned-byte-integer) %tail))
           )
   )

;;;-----------------------------------------------------------------------------
;;; providing some special objects to the compiler
;;;-----------------------------------------------------------------------------

;; for closures
(%annotate-function %closure-push is-special-function closure-push)
(%annotate-function %closure-value is-special-function closure-value)
(%annotate-function %set-closure-value is-special-function set-closure-value)
(%annotate-function %make-function is-special-function make-function)

(%annotate-binding unwind is-special-binding unwind)
(%annotate-binding stop-unwind-before is-special-binding stop-unwind-before)
(%annotate-binding continue-at is-special-binding continue-at)
(%annotate-function unwind-continue is-special-function unwind-continue)
(%annotate-binding letcc-result is-special-binding letcc-result)
(%annotate-class <dynamic> is-special-class <dynamic>)
(%annotate-binding top-dynamic is-special-binding top-dynamic)
(%annotate-function %dynamic is-special-function get-dynamic)
(%annotate-function %dynamic-setq is-special-function set-dynamic)
(%annotate-function make-dynamic is-special-function make-dynamic)

)  ;; End of module apply
