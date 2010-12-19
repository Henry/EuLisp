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
;;; Title: Trace functionality for EuXLisp
;;;  Authors: Russell Bradford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule trace
  (import (level-0)
   export (trace
           untrace))

(defconstant trace-table (make-table eq))

(defsyntax trace (name)
  `(progn
     (setq old ,name)
     (defun ,name args
       (format "-> ~s    ~s~%" ',name args)
       (let ((result (apply old args)))
         (format "~s ->    ~s~%" ',name result)
         result))
     ((setter table-ref) trace-table ,name old)
     ',name))

(defsyntax untrace (name)
  `(let ((old (table-ref trace-table ,name)))
     (if old
         (progn
           (setq ,name old)
           ((setter table-ref) trace-table ,name ())
           ',name)
       #f)))

;;;-----------------------------------------------------------------------------
)  ;; End of module trace
;;;-----------------------------------------------------------------------------
