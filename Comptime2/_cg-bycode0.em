;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Description: help to define instructions of the virtual machine
;;; -----------------------------------------------------------------------
(defmodule _cg-bycode0
  (syntax (macros)
   import (level1))
;;; --------------------------------------------------------------------
;;; General instructions
;;; --------------------------------------------------------------------
  (defmacro def-bytecode (name args code . properties)
    `((setter get-bytecode) ',name
      (make <bytecode>
            name: ',name
            args: ',args
            code: ,code
            properties: ',properties)))
;;; --------------------------------------------------------------------
;;; Registers
;;; --------------------------------------------------------------------
  (defmacro def-register (name code)
    `((setter get-register) ',name ,code))
)  ; end of module
