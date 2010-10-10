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
;;; Title: Scheme test
;;;  Library: scheme
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule test1
  (syntax (scheme0)
   import (scheme)
   export ())

(define (tak x y z)
        (if (not (< y x))
            z
          (tak (tak (- x 1) y z)
               (tak (- y 1) z x)
               (tak (- z 1) x y))))

(display (tak 18 12 6))

(newline)

;;;-----------------------------------------------------------------------------
)  ;; End of module test1
;;;-----------------------------------------------------------------------------
