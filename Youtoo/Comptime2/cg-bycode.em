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
;;; Title: instruction set of the virtual machine (see Vm/bytecodes.h)
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule cg-bycode
  (syntax (_macros
           _cg-bycode0
           _sx-obj0)
   import (i-level1)
   export (get-bytecode
           get-register
           get-stream-primitive))

;;;-----------------------------------------------------------------------------
;;; Initialization
;;;-----------------------------------------------------------------------------
(def-syntax-obj <bytecode> () (name args code properties size modus))
(defconstant get-bytecode (make-access-table))
(defconstant get-register (make-access-table))
(defconstant get-stream-primitive (make-access-table))

;;;-----------------------------------------------------------------------------
)  ;; End of module cg-bycode
;;;-----------------------------------------------------------------------------
