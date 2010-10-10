;;; Copyright 1995 Tracy Gardner & University of Bath
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
;;; Title: Main module
;;;  Library: ops5
;;;  Authors: Tracy Gardner
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------

(defmodule ops5
  (syntax (syntax-0
           macros-tag)
   import (level-0
           basic
           conflict
           cond-el-gf
           wm
           reader
           ops5-def)
   expose (conflict
           cond-el-gf
           wm
           reader
           ops5-def)
   export (ops-load))

;(let/cc exit
;(let loop ((x ()))
;(format "Enter name of OPS5 file: ")
;(let/cc restart
;(with-handler
;(lambda (c k)
;(output-condition-contents c)
;(restart ()))
;(setq x (read lispin () (eos-default-value)))
;(if (eq x (eos-default-value))
;(progn (print "Exiting" nl) (exit 0))
;(ops-load x))))
;(loop x)))

(print "### ops5" nl)

(defun ops-load (filename)
  (let ((reader (read-ops-prog (symbol-name filename)))
        (ops-sys (make <ops5-system>)))
    (print-ces (ce-man reader))
    (set-ce-manager ops-sys (ce-man reader))
    (print "OPS5: Network has been initialised" nl)
    (insert-wme (wm-manager ops-sys) (ce-manager ops-sys)
                (cr-manager ops-sys) 'start ())
    (print "OPS5: Start working memory element inserted" nl)
    (fire-prod-inst (cr-manager ops-sys) (wm-manager ops-sys)
                    (ce-manager ops-sys))))

;;;-----------------------------------------------------------------------------
)  ;; End of module ops5
;;;-----------------------------------------------------------------------------
