;;; EuLisp system 'youtoo'
;;;   Interface file for module boot0

(definterface boot0
  (import (level1)
   syntax ()
   full-import (read symbol random handler table table1 convert1 format list socket stream2 lock stream1 stream vector stream3 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos level1)
   export (
    ((name . get-global-register) (pos . 5) (origin boot0 . get-global-register))
    ((name . unless) (pos . 7) (origin boot0 . unless))
    ((name . set-global-register) (pos . 6) (origin boot0 . set-global-register))
    ((name . and) (pos . 4) (origin boot0 . and))
    ((name . when) (pos . 2) (origin boot0 . when))
    ((name . or) (pos . 8) (origin boot0 . or))
    ((name . cond) (pos . 3) (origin boot0 . cond))
   )
   local-literals (
    (when . 29)
    (get-global-register . 28)
    (set-global-register . 27)
    (unless . 26)
    (let . 24)
    (set-register-ref . 21)
    (x . 20)
    (opencoded-lambda . 18)
    (register-ref . 17)
    (and . 15)
    (or . 13)
    (cond . 12)
    (if . 10)
    (progn . 9)
   )
   literals (
   )
))
