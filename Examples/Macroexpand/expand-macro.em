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
;;; Title: Simple macroexpand example
;;;  Authors: Henry G. Weller
;;;  Maintainer: Henry G. Weller
;;;  Compilation
;;     youtoo test-expand-macro -l level-0
;;;-----------------------------------------------------------------------------

(defmodule expand-macro
  (syntax (syntax-0
           macro)
   import (level-0
           eval))

(print "Expanding (when t 1): " (macroexpand '(when t 1)) nl)
(print "Expanding (mac 1): " (macroexpand '(mac 1)) nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module expand-macro
;;;-----------------------------------------------------------------------------
