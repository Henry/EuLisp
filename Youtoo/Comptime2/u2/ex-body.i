;;; EuLisp system 'youtoo'
;;;   Interface file for module ex-body

(definterface ex-body
  (import (i-all p-env ex-expr sx-obj sx-node)
   syntax (_macros _sx-obj0)
   full-import (i-error i-notify i-param i-level-1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level-1 aux-table i-all sx-obj sx-obj1 sx-obj2 p-env cg-dld ex-direct ex-expose ex-syntax ex-import i-modify cg-interf sx-node i-ffi ex-expr)
   export (
    ((name . expand-bodies) (pos . 6) (origin ex-body . expand-bodies))
   )
   local-literals (
    (complete-variable-node . 32)
    (complete-constant-node . 31)
    (set-up-top-level-lambda . 30)
    (complete-top-level-forms . 29)
    (expand-bodies . 28)
    ("   Expand top-level forms" . 26)
    (anonymous . 25)
    ("   Complete defun bodies" . 24)
    ("   Complete constant nodes" . 23)
    ("   Complete variable nodes" . 22)
    ("Top-level" . 20)
    ("Top-level" . 18)
    (progn . 17)
    ("    Top-level forms: ~a" . 16)
    (args: . 15)
    (fun: . 14)
    (*encl-lambda* . 12)
    (top-level . 11)
    (setq . 8)
    (*actual-module* . 7)
   )
   literals (
   )
))
