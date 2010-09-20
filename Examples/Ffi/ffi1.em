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
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: foreign function test
;;;  Compilation
;;    youtoo ffi1 -l level1
;;;-----------------------------------------------------------------------------
(defmodule ffi1
  (syntax (macros)
   import (level1))

;;;-----------------------------------------------------------------------------
;;; Make use of C's atoi function
;;;-----------------------------------------------------------------------------
(defextern lisp-atoi (<string>) <int> "atoi")

(if (< 1 *argc*)
    (progn
      (print (+ (lisp-atoi (vector-ref *argv* 1))
                (lisp-atoi "123")))
      0) ;; Return a no error
  (error <condition> "no parameter passed"))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
