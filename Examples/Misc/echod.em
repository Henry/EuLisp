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
;;; Title: echod daemon
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;    youtoo echod -l level-1
;;;-----------------------------------------------------------------------------

(defmodule echod
  (syntax (macros)
   import (level-1))

(let* ((port (if (< *argc* 2) 4711 (vector-ref *argv* 1)))
       (s (make <socket> port: port))
       c x)
  (unwind-protect
      (while (setq c (make <connection> socket: s))
        (while (null? (eq (setq x (read-line c () (eos-default-value)))
                          (eos-default-value)))
          (format "echoing ~a" x)
          (sprint c x))
        (disconnect c))
    (disconnect s)))

;;;-----------------------------------------------------------------------------
)  ;; End of module echod
;;;-----------------------------------------------------------------------------
