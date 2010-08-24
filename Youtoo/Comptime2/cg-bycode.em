;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: instruction set of the virtual machine (see Vm/bytecodes.h)

;;;-----------------------------------------------------------------------------
(defmodule cg-bycode
  (syntax (_macros _cg-bycode0 _sx-obj0)
   import (i-level1)
   export (get-bytecode get-register get-stream-primitive))

;;;-----------------------------------------------------------------------------
;;; Initialization
;;;-----------------------------------------------------------------------------
(def-syntax-obj <bytecode> () (name args code properties size modus))
(defconstant get-bytecode (make-access-table))
(defconstant get-register (make-access-table))
(defconstant get-stream-primitive (make-access-table))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
