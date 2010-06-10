;;; EuLisp system 'youtoo'
;;;   Library interface file for module mpis

(definterface mpis
  (import ()
   syntax ()
   full-import (mpis read symbol random handler table table1 convert1 format list socket stream2 lock stream1 stream vector stream3 float character compare collect fpi number integer copy convert string callback let-cc dynamic thread event condition bit mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos level1 serial)
   export (
    ((name . local-mpi-stream?) (pos . 13) (origin mpis . local-mpi-stream?))
    ((name . <mpi-stream>) (pos . 9) (origin mpis . <mpi-stream>) (class . constant))
    ((name . mpi-stream-p) (pos . 6) (origin mpis . mpi-stream-p))
    ((name . mpi-stream-host) (pos . 2) (origin mpis . mpi-stream-host) (inline (G00116 (static-fpi-ref 3) (binding-ref ? <mpi-stream>) (primitive-relative-ref))) (setter (G00124 (stack-ref 1) (static-fpi-ref 3) (stack-ref 2) (binding-ref ? <mpi-stream>) (set-primitive-relative-ref) (nobble 2))))
    ((name . mpi-stream-rank) (pos . 3) (origin mpis . mpi-stream-rank) (inline (G00114 (static-ref2) (binding-ref ? <mpi-stream>) (primitive-relative-ref))) (setter (G00122 (stack-ref 1) (static-ref2) (stack-ref 2) (binding-ref ? <mpi-stream>) (set-primitive-relative-ref) (nobble 2))))
   )
   literals (
   )
  )
)  ; end of interface