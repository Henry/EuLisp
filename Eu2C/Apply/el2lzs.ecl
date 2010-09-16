;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: The interface of the Eulisp-to-LZS-Transformer
;;;  Description:
;;    This module provides a common interface for the transformation from EuLisp to LZS.
;;;  Documentation:
;;;  Notes:
;;    The module lzs-modules should be included into this module and should then
;;    deleted.
;;;  Requires:
;;;  Problems:
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module el2lzs
(import
 (level-0 el2lzs-main)

 syntax
 (level-0 dynamic)

 export   ;; for interactive use
 (load-module             ;;(module-id | path | "") loads and translates a module
  find-module
  module-env  ; the list of all modules needed for compilation
  mark-as-exported
  get-identifier-and-object
  )

 expose
 (lzs-modules)
 )

(defun make-lzs-expression (form environment)
  (dynamic-let ((lex-env environment))
               (trans form)))

#module-end

