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
;;; Title: A scheme-like language implementation
;;;  Authors: Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    call/cc may be weird
;;    syntax of various objects may be different (e.g., symbols)
;;;-----------------------------------------------------------------------------

(defmodule scheme
  (import (schemer
           (rename
            ((error ERROR))
            (only
             (!>
              exit
              expose
              export
              error
              defcondition
              <error>)
             level-0)))
   export (!> exit error))

(expose schemer)

(defcondition <scheme-error> <error>)

(define (error a b)
        (ERROR
         a
         <scheme-error>
         value: b))

;;;-----------------------------------------------------------------------------
)  ;; End of module scheme
;;;-----------------------------------------------------------------------------
