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
;;; Title: Macros
;;;  Library: ops5
;;;  Authors: Tracy Gardner
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------

(defmodule macros-tag
  ()

; (syntax (syntax-0)
;        import (level-0))

;  (defmacro while (condition . body)
;    `(let/cc break                      ; (syntax break)
;       (letfuns ((| do it again | ()
;                 (when ,condition
;                      ,@body
;                      (| do it again |))))
;               (| do it again |))))

;  (defmacro for (init condition inc . body)
;    `(progn
;       ,init
;       (while ,condition
;         ,@body
;         ,inc)))

;;;-----------------------------------------------------------------------------
)  ;; End of module macros-tag
;;;-----------------------------------------------------------------------------
