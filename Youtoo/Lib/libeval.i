;;; EuLisp system 'youtoo'
;;;   Library interface file for module eval

(definterface eval
  (import ()
   syntax ()
   full-import (eval i-error i-notify i-param i-level1 boot1 boot symbol stream3 random handler read table table1 vector convert1 format list socket stream2 lock stream1 stream double double1 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl telos level1 aux-table i-all i-args cg-exec cg-exec-word-length cg-link cg-asm cg-bycode cg-bycode1 cg-bycode2 op-peep op-peep-r cg-gen cg-state cg-stack an-side p-parse ex-expr cg-dld ex-direct ex-expose ex-syntax ex-import cg-interf sx-node i-ffi ex-module ex-body sx-write p-env p-read sx-obj2 sx-obj1 sx-obj i-modify i-compile i-rep)
   export (
    ((name . dynamic-binding-set1) (pos . 12) (origin cg-dld . dynamic-binding-set1) (class . ff) (arity . 3) (ff (3 0 8) 6 (eul_dyn_binding_set . ff_stub_eul_dyn_binding_set5720)))
    ((name . module-loaded?) (pos . 6) (origin cg-dld . module-loaded?))
    ((name . ?) (pos . 15) (origin i-rep . ?))
    ((name . dynamic-binding-ref1) (pos . 11) (origin cg-dld . dynamic-binding-ref1) (class . ff) (arity . 2) (ff (3 0) 6 (eul_dyn_binding_ref . ff_stub_eul_dyn_binding_ref5719)))
    ((name . dynamic-load-module) (pos . 3) (origin cg-dld . dynamic-load-module))
    ((name . rep) (pos . 6) (origin i-rep . rep))
    ((name . as-C-module-name) (pos . 14) (origin cg-dld . as-C-module-name) (class . ff) (arity . 1) (ff (8) 3 (eul_module_name_as_C_module_name_string . ff_stub_eul_module_name_as_C_module_name_string5722)))
    ((name . as-dynamic-binding) (pos . 4) (origin cg-dld . as-dynamic-binding))
    ((name . main) (pos . 3) (origin eval . main))
    ((name . dynamic-load-module1) (pos . 13) (origin cg-dld . dynamic-load-module1) (class . ff) (arity . 2) (ff (3 3) 0 (eul_dyn_load_module . ff_stub_eul_dyn_load_module5721)))
    ((name . *redefine-imported-bindings*) (pos . 31) (origin i-param . *redefine-imported-bindings*))
    ((name . dynamic-binding-ref) (pos . 7) (origin cg-dld . dynamic-binding-ref))
    ((name . eval) (pos . 8) (origin i-rep . eval))
    ((name . dynamic-binding-set) (pos . 9) (origin cg-dld . dynamic-binding-set))
   )
   literals (
   )
  )
)  ; end of interface