;;; EuLisp system 'youtoo'
;;;   Interface file for module mop-init

(definterface mop-init
  (import (boot mop-prim mop-class)
   syntax (_boot0)
   full-import (boot1 boot mop-prim mop-class)
   export (
    ((name . init-class) (pos . 3) (origin mop-init . init-class) (inline (G00723 (stack-ref 7) (stack-ref 7) (stack-ref 1) (static-ref0) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 6) (stack-ref 1) (static-ref1) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 5) (stack-ref 1) (static-ref2) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 4) (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (static-ref-nil) (stack-ref 1) (static-fpi-ref 4) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 3) (stack-ref 1) (static-fpi-ref 5) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 2) (cons) (stack-ref 8) (stack-ref 1) (stack-ref 1) (static-fpi-ref 6) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 8) (stack-ref 2) (stack-ref 1) (static-fpi-ref 7) (stack-ref 2) (set-primitive-ref) (nobble 2) (nobble 1) (nobble 8))))
    ((name . compute-class-codes) (pos . 2) (origin mop-init . compute-class-codes))
   )
   local-literals (
    (top-level . 31)
    (compute-class-codes . 30)
    (init-class . 29)
    (anonymous . 25)
    (cons . 22)
    (null . 21)
    (list . 20)
    (keyword . 19)
    (symbol . 18)
    (name . 17)
    (local-slot . 16)
    (slot . 15)
    (simple-method . 14)
    (method . 13)
    (simple-generic-function . 12)
    (generic-function . 11)
    (simple-function . 10)
    (function . 9)
    (function-class . 8)
    (simple-class . 7)
    (class . 6)
    (metaclass . 5)
    (object . 4)
   )
   literals (
   )
))
