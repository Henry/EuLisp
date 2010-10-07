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
;;; Description: checking out the echo daemon (echod)
;;;  Compilation
;;    youtoo echo -l level1
;;;-----------------------------------------------------------------------------
(defmodule echo
  (syntax (macros)
   import (level1))

(let* ((port (if (< *argc* 2) 4711 (vector-ref *argv* 1)))
       (c (make <connection> port: port))
       x)
  (while (null? (eq (setq x (read-line stdin () (eos-default-value)))
                    (eos-default-value)))
    (sprint c x)
    (print (read-line c) nl))
  (disconnect c))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
