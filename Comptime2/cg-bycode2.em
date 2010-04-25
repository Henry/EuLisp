;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: instruction set of the virtual machine (see Vm/bytecodes.h)

;;;-----------------------------------------------------------------------------
(defmodule cg-bycode2
  (syntax (_macros _cg-bycode0 _sx-obj0)
   import (i-level1 cg-bycode)
   expose (cg-bycode))

;;;-----------------------------------------------------------------------------
;;; Branching
;;;-----------------------------------------------------------------------------
  (def-bytecode branch (label)                50)
  (def-bytecode branch-long-neg (byte)        75)
  (def-bytecode branch-neg (byte)             53)
  (def-bytecode branch-pos (byte)             54)
  (def-bytecode branch-long-pos (byte)        57)
  (def-bytecode branch-true (label)           51 (in obj))
  (def-bytecode branch-true-long-neg (byte)   76 (in obj))
  (def-bytecode branch-true-neg (byte)        58 (in obj))
  (def-bytecode branch-true-pos (byte)        62 (in obj))
  (def-bytecode branch-true-long-pos (byte)   63 (in obj))
  (def-bytecode branch-nil (label)            52 (in obj))
  (def-bytecode branch-nil-long-neg (byte)    77 (in obj))
  (def-bytecode branch-nil-neg (byte)         64 (in obj))
  (def-bytecode branch-nil-pos (byte)         68 (in obj))
  (def-bytecode branch-nil-long-pos (byte)    74 (in obj))

;;;-----------------------------------------------------------------------------
;;; Calling
;;;-----------------------------------------------------------------------------
  (def-bytecode call-next-method (byte)           55 (in *) (out obj))
  (def-bytecode tail-call-next-method (byte byte) 56 (in *) (out obj))
  (def-bytecode make-lambda (byte)                59 (in obj obj) (out obj))
  (def-bytecode call-operator (byte)              60 (in *) (out obj))
  (def-bytecode tail-call-operator (byte byte)    61 (in *) (out obj))
  (def-bytecode apply ()                         142 (in obj1 obj2) (out obj))
  (def-bytecode call-foreign-function (ff)        65 (in int *) (out obj))
  (def-bytecode write-object ()                   66 (in obj1 obj2) (out obj))
  (def-bytecode check-arguments (byte)            67)
  (def-bytecode check-arguments-2 ()             167)
  (def-bytecode check-arguments-1 ()             168)
  (def-bytecode check-arguments0 ()              169)
  (def-bytecode check-arguments1 ()              170)
  (def-bytecode check-arguments2 ()              171)
  (def-bytecode return (byte)                     69)
  (def-bytecode return0 ()                       172)
  (def-bytecode return1 ()                       173)
  (def-bytecode return2 ()                       174)

;;;-----------------------------------------------------------------------------
;;; Displays
;;;-----------------------------------------------------------------------------
  (def-bytecode alloc (byte)                70)
  (def-bytecode display-ref (byte byte)     71 (out obj))
  (def-bytecode set-display-ref (byte byte) 72 (in obj))
  (def-bytecode dealloc ()                  73)

;;;-----------------------------------------------------------------------------
;;; Logic operators
;;;-----------------------------------------------------------------------------
  (def-bytecode eq ()    80 (in obj1 obj2) (out obj))

;;;-----------------------------------------------------------------------------
;;; Locks
;;;-----------------------------------------------------------------------------
  (def-bytecode test-and-set-lock () 7 (in obj) (out obj))

;;;-----------------------------------------------------------------------------
;;; Unflush stacks
;;;-----------------------------------------------------------------------------
  (def-bytecode unflush-stacks () 88 (in) (out obj))

;;;-----------------------------------------------------------------------------
;;; States
;;;-----------------------------------------------------------------------------
  (def-bytecode fill-thread-state ()    90 (in obj1) (out obj1))
  (def-bytecode restore-thread-state () 91 (in obj1) (out obj1))
  (def-bytecode fill-state ()           92 (in obj1) (out obj1))
  (def-bytecode restore-state ()        93 (in obj1 obj2))
  (def-bytecode state-ref ()            95 (out obj))
  (def-bytecode set-state-ref ()        96 (in obj))

;;;-----------------------------------------------------------------------------
;;; Ascii conversion
;;;-----------------------------------------------------------------------------
  (def-bytecode character-as-fpi () 98 (in char) (out int))
  (def-bytecode fpi-as-character () 99 (in int)  (out char))

;;;-----------------------------------------------------------------------------
;;; Basic stuff
;;;-----------------------------------------------------------------------------
  (def-bytecode exit () 102 (in obj))

;;;-----------------------------------------------------------------------------
;;; Symbols and searching
;;;-----------------------------------------------------------------------------
  (def-bytecode intern () 105 (in obj1)           (out obj2))
  (def-bytecode assq ()   106 (in obj1 obj2 obj3) (out obj3))
  (def-bytecode iniq ()   107 (in obj1 obj2 obj3) (out obj3))
  (def-bytecode memq ()   108 (in obj1 obj2 obj3) (out obj3))

;;;-----------------------------------------------------------------------------
;;; Signals
;;;-----------------------------------------------------------------------------
  ; (def-bytecode enable-time-slice () 113 (in int) (out int))
  ; (def-bytecode enable-signals ()    110          (out obj))
  ; (def-bytecode disable-signals ()   111          (out obj))
  ; (def-bytecode block-on-signals ()  112          (out obj))

;;;-----------------------------------------------------------------------------
;;; Register Codes
;;;-----------------------------------------------------------------------------
  (def-register argc              22)
  (def-register argv              23)
  (def-register byte-vector-class 3)
  (def-register character-class   10)
  (def-register cons-class        4)
  (def-register double-class      6)
  (def-register double-ref-class  41)
  ;  (def-register env-class         11)
  (def-register fpi-class         2)
  (def-register fpi-ref-class     40)
  (def-register generic-class    19)
  (def-register keyword-class    20)
  (def-register lambda-class     18)
  (def-register method-class     13)
  (def-register null-class       25)
  (def-register string-class     15)
  (def-register string-ref-class 42)
  (def-register symbol-class     16)
  (def-register table-class      28)
  (def-register vector-class     26)
  (def-register callbacks        14)
  (def-register next-methods     24)
  (def-register keywords         27)
  (def-register symbols          17)
  ;  (def-register argc              0)
  ;  (def-register argv              1)
  ;  (def-register byte-vector-class 2)
  ;  (def-register character-class   3)
  ;  (def-register cons-class        4)
  ;  (def-register double-class      5)
  ;  (def-register double-ref-class  6)
  ;;  (def-register env-class         7)
  ;  (def-register fpi-class         8)
  ;  (def-register fpi-ref-class     9)
  ;  (def-register generic-class    10)
  ;  (def-register keyword-class    11)
  ;  (def-register lambda-class     12)
  ;  (def-register method-class     13)
  ;  (def-register null-class       14)
  ;  (def-register string-class     15)
  ;  (def-register string-ref-class 16)
  ;  (def-register symbol-class     17)
  ;  (def-register table-class      18)
  ;  (def-register vector-class     19)
  ;  (def-register callbacks        10)
  ;  (def-register next-methods     21)
  ;  (def-register keywords         22)
  ;  (def-register symbols          23)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
