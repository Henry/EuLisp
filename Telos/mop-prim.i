;;; EuLisp system 'youtoo'
;;;   Interface file for module mop-prim

(definterface mop-prim
  (import (boot)
   syntax ()
   full-import (boot1 boot)
   export (
    ((name . primitive-ref) (pos . 4) (origin mop-prim . primitive-ref) (inline (G00556 (primitive-ref))) (setter (G00560 (set-primitive-ref))))
    ((name . primitive-allocate) (pos . 3) (origin mop-prim . primitive-allocate) (inline (G00552 (primitive-allocate))))
    ((name . primitive-class-of) (pos . 2) (origin mop-prim . primitive-class-of) (inline (G00554 (primitive-class-of))) (setter (G00558 (set-primitive-class-of))))
   )
   local-literals (
    (top-level . 16)
    (primitive-class-of . 15)
    (primitive-allocate . 14)
    (primitive-ref . 13)
    (|(setter primitive-ref)| . 8)
    (|(setter primitive-class-of)| . 7)
   )
   literals (
   )
))
