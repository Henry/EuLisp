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
;;; Description: Common Lisp like dispatch macro characters
;;;  Compilation
;;    Needs file macro-dispatch-char.dat
;;    with (1 2 3 #M(4 5 6) 7 8 9)
;;;-----------------------------------------------------------------------------
(defmodule macro-dispatch-char
  (syntax (macros)
   import (level1)
   export ())

(set-dispatch-macro-character #\# #\M
                              (lambda (s key dummy)
                                (reverse (read-s-expression s))))

(let ((file (make <file-stream> file-name: "macro-dispatch-char.dat")))
  (print (read-s-expression file) nl)
  (disconnect file))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
