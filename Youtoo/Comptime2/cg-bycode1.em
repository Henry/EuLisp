;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: instruction set of the virtual machine (see Vm/bytecodes.h)

;;;-----------------------------------------------------------------------------
(defmodule cg-bycode1
  (syntax (_macros _cg-bycode0 _sx-obj0)
   import (i-level1 cg-bycode)
   expose (cg-bycode))

;;;-----------------------------------------------------------------------------
;;; No operation
;;;-----------------------------------------------------------------------------
(def-bytecode noop () 0)

;;;-----------------------------------------------------------------------------
;;; Primitives
;;;-----------------------------------------------------------------------------
(def-bytecode primitive-allocate ()     1 (in class int)      (out obj))
(def-bytecode primitive-ref ()          2 (in obj1 int)       (out obj2))
(def-bytecode primitive-relative-ref () 8 (in obj1 int class) (out obj2))
(def-bytecode set-primitive-ref ()      3 (in obj1 int val))

(def-bytecode set-primitive-relative-ref () 9 (in obj1 int1 obj2 class)
              (out obj2))

(def-bytecode primitive-class-of ()     4 (in obj)            (out class))
(def-bytecode set-primitive-class-of () 5 (in obj class))
(def-bytecode primitive-size ()         6 (in obj)            (out int))

;;;-----------------------------------------------------------------------------
;;; Strings
;;;-----------------------------------------------------------------------------
(def-bytecode string-ref ()      11 (in str int)     (out char))
(def-bytecode set-string-ref ()  12 (in str int char)(out char))

;;;-----------------------------------------------------------------------------
;;; Lists
;;;-----------------------------------------------------------------------------
(def-bytecode cons ()         15 (in obj obj) (out cons))
(def-bytecode car ()          16 (in cons)    (out obj))
(def-bytecode cdr ()          17 (in cons)    (out obj))
(def-bytecode CAR ()          13 (in cons)    (out obj))
(def-bytecode CDR ()          14 (in cons)    (out obj))
(def-bytecode caar ()        114 (in cons)    (out obj))
(def-bytecode cadr ()        115 (in cons)    (out obj))
(def-bytecode cdar ()        116 (in cons)    (out obj))
(def-bytecode cddr ()        117 (in cons)    (out obj))
(def-bytecode caddr ()       118 (in cons)    (out obj))
(def-bytecode cadddr ()      119 (in cons)    (out obj))
(def-bytecode set-car ()     143 (in cons obj)(out obj))
(def-bytecode set-cdr ()     144 (in cons obj)(out obj))
(def-bytecode listify (byte)  19 (in int)     (out cons))

;;;-----------------------------------------------------------------------------
;;; Class membership
;;;-----------------------------------------------------------------------------
(def-bytecode null ()         18 (in obj)     (out obj))
(def-bytecode consp ()       122 (in obj)     (out obj))
(def-bytecode listp ()       123 (in obj)     (out obj))
(def-bytecode symbolp ()     124 (in obj)     (out obj))
(def-bytecode stringp ()     125 (in obj)     (out obj))
(def-bytecode fpip ()        126 (in obj)     (out obj))
(def-bytecode lambdap ()     127 (in obj)     (out obj))
(def-bytecode gfp ()         128 (in obj)     (out obj))
(def-bytecode characterp ()  129 (in obj)     (out obj))

;;;-----------------------------------------------------------------------------
;;; Setter
;;;-----------------------------------------------------------------------------
(def-bytecode setter ()      145 (in obj)     (out obj))
(def-bytecode set-setter ()  146 (in obj obj) (out obj))

;;;-----------------------------------------------------------------------------
;;; Fpis
;;;-----------------------------------------------------------------------------
(def-bytecode fpi-sum ()         20 (in int int) (out int))
(def-bytecode fpi-difference ()  21 (in int int) (out int))
(def-bytecode fpi-product ()     22 (in int int) (out int))
(def-bytecode fpi-quotient ()    23 (in int int) (out int))
(def-bytecode fpi-remainder ()   24 (in int int) (out int))
(def-bytecode fpi-equal ()       25 (in int int) (out obj))
(def-bytecode fpi-lt ()          26 (in int int) (out obj))
(def-bytecode fpi-inc ()         43 (in int) (out int))
(def-bytecode fpi-dec ()         44 (in int) (out int))
(def-bytecode fpi-zerop ()       45 (in int) (out obj))

;;;-----------------------------------------------------------------------------
;;; Stack (keep losing should supercede nobble and take its op code)
;;;-----------------------------------------------------------------------------
(def-bytecode stack-ref0 ()           27 (out obj))
(def-bytecode stack-ref1 ()           28 (out obj))
(def-bytecode stack-ref2 ()           29 (out obj))
(def-bytecode swap ()                 30)
(def-bytecode stack-ref (byte)        31 (out obj))
(def-bytecode set-stack-ref (byte)    32 (in obj))
(def-bytecode pop (byte)              33 (in parameter))
(def-bytecode pop1 ()                 42 (in parameter))
(def-bytecode nobble (byte)           34)

;;;-----------------------------------------------------------------------------
;;; Bindings and literals
;;;-----------------------------------------------------------------------------
(def-bytecode static-ref0 ()                130 (out obj))
(def-bytecode static-ref1 ()                131 (out obj))
(def-bytecode static-ref2 ()                132 (out obj))
(def-bytecode static-ref-1 ()               133 (out obj))
(def-bytecode static-ref-nil ()             134 (out obj))
(def-bytecode static-ref-t ()               135 (out obj))
(def-bytecode static-ref (static)            35 (out obj))
(def-bytecode binding-ref (int)              36 (out obj))
(def-bytecode set-binding-ref (int)          37 (in obj))
(def-bytecode set-and-get-binding-ref (int) 137 (in obj) (out obj))
(def-bytecode static-fpi-ref (int)           38 (out obj))
(def-bytecode static-fpi-byte-ref (byte)    138 (out obj))
(def-bytecode static-character-ref (byte)    39 (out obj))

;;;-----------------------------------------------------------------------------
;;; Registers
;;;-----------------------------------------------------------------------------
(def-bytecode register-ref (reg)     40 (out obj))
(def-bytecode set-register-ref (reg) 41 (in obj))

;;;-----------------------------------------------------------------------------
;;; Access to context stack
;;;-----------------------------------------------------------------------------
(def-bytecode context-stack-ref ()    140 (in obj) (out obj))
(def-bytecode value-stack-ref ()      141 (in obj obj) (out obj))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
