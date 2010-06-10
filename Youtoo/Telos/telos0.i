;;; EuLisp system 'youtoo'
;;;   Interface file for module telos0

(definterface telos0
  (import (boot0 mop-gf0 mop-meth0 mop-defcl0)
   syntax ()
   full-import (level1 telos boot1 boot mop-defcl mop-meth mop-gf mop-inspect mop-init mop-class mop-key mop-prim mop-access mop-alloc bit condition event thread dynamic let-cc callback string convert copy integer number fpi collect compare character float stream stream1 lock stream2 socket list format convert1 vector table1 table read handler random stream3 symbol boot0 mop-gf0 mop-meth0 mop-defcl0)
   export (
    ((name . defgeneric) (pos . 2) (origin mop-gf0 . defgeneric))
    ((name . defmethod-sig) (pos . 8) (origin mop-meth0 . defmethod-sig))
    ((name . defmethod-domain) (pos . 4) (origin mop-meth0 . defmethod-domain))
    ((name . generic-lambda) (pos . 5) (origin mop-gf0 . generic-lambda))
    ((name . defmethod) (pos . 5) (origin mop-meth0 . defmethod))
    ((name . cond) (pos . 3) (origin boot0 . cond))
    ((name . method-lambda) (pos . 3) (origin mop-meth0 . method-lambda))
    ((name . method-function-lambda) (pos . 6) (origin mop-meth0 . method-function-lambda))
    ((name . defclass) (pos . 11) (origin mop-defcl0 . defclass))
    ((name . defprimclass) (pos . 5) (origin mop-defcl0 . defprimclass))
    ((name . defmethod-body) (pos . 7) (origin mop-meth0 . defmethod-body))
    ((name . get-global-register) (pos . 5) (origin boot0 . get-global-register))
    ((name . unless) (pos . 7) (origin boot0 . unless))
    ((name . set-global-register) (pos . 6) (origin boot0 . set-global-register))
    ((name . and) (pos . 4) (origin boot0 . and))
    ((name . when) (pos . 2) (origin boot0 . when))
    ((name . defmethod-args) (pos . 2) (origin mop-meth0 . defmethod-args))
    ((name . or) (pos . 8) (origin boot0 . or))
    ((name . named-method-function-lambda) (pos . 10) (origin mop-meth0 . named-method-function-lambda))
    ((name . defmethod-keywords) (pos . 9) (origin mop-meth0 . defmethod-keywords))
   )
   local-literals (
   )
   literals (
   )
))
