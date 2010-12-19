;;; Copyright 2010 Henry G. Weller and Stefan Israelsson Tampe
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
;;; Title: Macros for test-smatch
;;;  Library: smatch
;;;  Authors: Henry G. Weller and Stefan Israelsson Tampe
;;;  Maintainer: Henry G. Weller and Stefan Israelsson Tampe
;;;  Description:
;;    See test-smatch.em
;;;-----------------------------------------------------------------------------

(defmodule test-smatch-syntax
  (syntax (syntax-0)
   import (level-0
           eval))

(defsyntax print-test (body)
  `(print (fmt "~s" ',body) " => " (fmt "~s" ,body) nl))

;; (print (expand-syntax '(smatch x ((set s) (s 4)))) nl)
;; (print (expand-syntax '(smatch x ((get s) (s)))) nl)
;; (print (expand-syntax '(defmatchfun (setter hmm) ((a (set s)) (s 4)))) nl)
;; (print (expand-syntax '(smatch x ((a ... b c) (list 'var-a...bc a b c)))) nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module test-smatch-syntax
;;;-----------------------------------------------------------------------------
