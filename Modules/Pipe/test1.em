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
;;;  Library: pipe
;;;  Authors: Andreas Kind
;;; Description: example pipe
;;;  Compilation
;;    make distclean
;;    make
;;    make test
;;;-----------------------------------------------------------------------------
(defmodule test1
  (syntax (macros)
   import (level1 pipe))

(let ((x (make <pipe> process: "../../Bin.x86_64/youtoo.sh"))
      (str ""))
  (pprint x)
  (system "ps -a")
  (sprint x (+ 1 3) nl)
  (sprint x hierarchy: nl)
  (sprint x exit: nl)
  (while (setq str (read-line x () ()))
    (print str)
    (flush)))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
