;;; EuLisp system 'youtoo'
;;;   Interface file for module mop-init

(definterface mop-init
  (import (boot mop-prim mop-class)
   syntax (_boot0)
   full-import (boot1 boot mop-prim mop-class)
   export (
    ((name . init-class) (pos . 3) (origin mop-init . init-class) (inline (G001330 (stack-ref 7) (stack-ref 7) (stack-ref 1) (static-ref0) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 6) (stack-ref 1) (static-ref1) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 5) (stack-ref 1) (static-ref2) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 4) (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (static-ref-nil) (stack-ref 1) (static-fpi-ref 4) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 3) (stack-ref 1) (static-fpi-ref 5) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 7) (stack-ref 2) (cons) (stack-ref 8) (stack-ref 1) (stack-ref 1) (static-fpi-ref 6) (stack-ref 2) (set-primitive-ref) (nobble 2) (pop1) (stack-ref 8) (stack-ref 2) (stack-ref 1) (static-fpi-ref 7) (stack-ref 2) (set-primitive-ref) (nobble 2) (nobble 1) (nobble 8))))
    ((name . compute-class-codes) (pos . 2) (origin mop-init . compute-class-codes))
   )
   local-literals (
    (top-level . 30)
    (compute-class-codes . 29)
    (init-class . 28)
    (anonymous . 24)
    (cons . 21)
    (null . 20)
    (list . 19)
    (keyword . 18)
    (symbol . 17)
    (name . 16)
    (local-slot . 15)
    (slot . 14)
    (simple-method . 13)
    (method . 12)
    (simple-generic-function . 11)
    (generic-function . 10)
    (simple-function . 9)
    (function . 8)
    (function-class . 7)
    (simple-class . 6)
    (class . 5)
    (object . 4)
   )
   literals (
   )
))
