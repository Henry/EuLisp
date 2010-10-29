;;; EuLisp system 'youtoo'
;;;   Interface file for module sx-node

(definterface sx-node
  (import (i-all i-ffi sx-obj p-env)
   syntax (_macros _i-aux0 _sx-obj0)
   full-import (i-error i-notify i-param i-level-1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level-1 aux-table i-all i-ffi sx-obj2 sx-obj1 sx-obj p-env)
   export (
    ((name . register-binding-ref) (pos . 25) (origin sx-node . register-binding-ref))
    ((name . make-defined-opencoded-fun) (pos . 10) (origin sx-node . make-defined-opencoded-fun))
    ((name . make-mutable-binding) (pos . 15) (origin sx-node . make-mutable-binding))
    ((name . make-inlined-setter) (pos . 26) (origin sx-node . make-inlined-setter))
    ((name . clone-node) (pos . 21) (origin sx-node . clone-node))
    ((name . compute-arity) (pos . 7) (origin sx-node . compute-arity))
    ((name . make-defined-fun) (pos . 6) (origin sx-node . make-defined-fun))
    ((name . make-dummy-binding) (pos . 5) (origin sx-node . make-dummy-binding))
    ((name . true-local-binding?) (pos . 9) (origin sx-node . true-local-binding?))
    ((name . make-let*) (pos . 19) (origin sx-node . make-let*))
    ((name . make-module) (pos . 3) (origin sx-node . make-module))
    ((name . non-folded-local-binding?) (pos . 27) (origin sx-node . non-folded-local-binding?))
    ((name . make-global-var) (pos . 17) (origin sx-node . make-global-var))
    ((name . make-setq) (pos . 12) (origin sx-node . make-setq))
    ((name . register-delegated-vars) (pos . 2) (origin sx-node . register-delegated-vars))
    ((name . get-binding-info) (pos . 20) (origin sx-node . get-binding-info))
    ((name . make-named-const) (pos . 16) (origin sx-node . make-named-const))
    ((name . binding-origin-module-name) (pos . 28) (origin sx-node . binding-origin-module-name))
    ((name . make-defined-external-fun) (pos . 18) (origin sx-node . make-defined-external-fun))
    ((name . make-fun) (pos . 8) (origin sx-node . make-fun))
    ((name . get-inlined-setter-binding) (pos . 24) (origin sx-node . get-inlined-setter-binding))
    ((name . get-binding-spec-info) (pos . 11) (origin sx-node . get-binding-spec-info))
    ((name . make-immutable-binding) (pos . 22) (origin sx-node . make-immutable-binding))
    ((name . make-local-static-var) (pos . 13) (origin sx-node . make-local-static-var))
   )
   local-literals (
    (top-level . 126)
    (register-delegated-vars . 125)
    (make-module . 124)
    (get-imported-inlined-setter-binding . 123)
    (make-dummy-binding . 122)
    (make-defined-fun . 121)
    (compute-arity . 120)
    (make-fun . 119)
    (true-local-binding? . 118)
    (make-defined-opencoded-fun . 117)
    (get-binding-spec-info . 116)
    (make-setq . 115)
    (make-local-static-var . 114)
    (foldable-constant? . 113)
    (make-mutable-binding . 112)
    (make-named-const . 111)
    (make-global-var . 110)
    (make-defined-external-fun . 109)
    (make-let* . 108)
    (get-binding-info . 107)
    (make-immutable-binding . 106)
    (make-binding . 105)
    (get-inlined-setter-binding . 104)
    (register-binding-ref . 103)
    (make-inlined-setter . 102)
    (non-folded-local-binding? . 101)
    (((class . constant)) . 87)
    (name . 85)
    (used: . 82)
    (value: . 81)
    (binding: . 79)
    (arity . 76)
    (value . 74)
    (constant . 73)
    (class . 72)
    (ff . 71)
    (opencoding . 70)
    (has-unknown-appls: . 68)
    (body: . 67)
    (arity: . 66)
    (args: . 65)
    (progn . 64)
    (| unbound | . 60)
    (*actual-module* . 58)
    (inline . 57)
    (setter . 56)
    ("no lexical binding ~a available" . 55)
    (syntax-env: . 53)
    (external-env: . 52)
    (interactive-lexical-env: . 51)
    (lexical-env: . 50)
    (name: . 49)
    (*encl-lambda* . 47)
    (|(method clone-node)| . 45)
    (|(method get-named-encl-lambda)| . 44)
    (|(method binding-origin-module-name)| . 43)
    (clone-node . 42)
    (binding-origin-module-name . 41)
    (anonymous . 37)
    (info: . 35)
    (local-index: . 34)
    (obj: . 33)
    (imported: . 32)
    (immutable: . 31)
    (module: . 30)
    (local-name: . 29)
   )
   literals (
   )
))
