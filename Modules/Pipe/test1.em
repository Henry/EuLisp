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
;;; Title: Test pipe
;;;  Library: pipe
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation:
;;    make distclean
;;    make
;;    make test
;;;-----------------------------------------------------------------------------

(defmodule test1
  (syntax (syntax-0)
   import (level-0
           pipe))

(let ((x (make <pipe> process: "../../Bin.x86_64/youtoo"))
      (str ""))
  (pprint x)
  (system "ps -a")
  (sprint x nl)
  (sprint x (+ 1 3) nl)
  (sprint x hierarchy: nl)
  (sprint x exit: nl)
  (while (setq str (read-line x () ()))
    (print str)
    (flush)))

;;;-----------------------------------------------------------------------------
)  ;; End of module test1
;;;-----------------------------------------------------------------------------
