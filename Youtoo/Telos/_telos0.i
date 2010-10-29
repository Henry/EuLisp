;;; EuLisp system 'youtoo'
;;;   Interface file for module _telos0

(definterface _telos0
  (import (_boot0 _mop-gf0 _mop-meth0 _mop-defcl0)
   syntax ()
   full-import (level-1 telos boot1 boot mop-defcl mop-meth mop-gf mop-inspect mop-init mop-class mop-key mop-prim mop-access mop-alloc bit condition event thread dynamic let-cc callback string convert copy integer number fpi collect compare character float stream stream1 lock stream2 socket list format convert1 vector table1 table read handler random stream3 symbol _boot0 _mop-gf0 _mop-meth0 _mop-defcl0)
   export (
    ((name . defgeneric) (pos . 2) (origin _mop-gf0 . defgeneric))
    ((name . defmethod-sig) (pos . 8) (origin _mop-meth0 . defmethod-sig))
    ((name . defmethod-domain) (pos . 4) (origin _mop-meth0 . defmethod-domain))
    ((name . generic-lambda) (pos . 5) (origin _mop-gf0 . generic-lambda))
    ((name . defmethod) (pos . 5) (origin _mop-meth0 . defmethod))
    ((name . cond) (pos . 3) (origin _boot0 . cond))
    ((name . method-lambda) (pos . 3) (origin _mop-meth0 . method-lambda))
    ((name . method-function-lambda) (pos . 6) (origin _mop-meth0 . method-function-lambda))
    ((name . defclass) (pos . 11) (origin _mop-defcl0 . defclass))
    ((name . defprimclass) (pos . 5) (origin _mop-defcl0 . defprimclass))
    ((name . defmethod-body) (pos . 7) (origin _mop-meth0 . defmethod-body))
    ((name . get-global-register) (pos . 5) (origin _boot0 . get-global-register))
    ((name . unless) (pos . 7) (origin _boot0 . unless))
    ((name . set-global-register) (pos . 6) (origin _boot0 . set-global-register))
    ((name . and) (pos . 4) (origin _boot0 . and))
    ((name . when) (pos . 2) (origin _boot0 . when))
    ((name . defmethod-args) (pos . 2) (origin _mop-meth0 . defmethod-args))
    ((name . or) (pos . 8) (origin _boot0 . or))
    ((name . named-method-function-lambda) (pos . 10) (origin _mop-meth0 . named-method-function-lambda))
    ((name . defmethod-keywords) (pos . 9) (origin _mop-meth0 . defmethod-keywords))
   )
   local-literals (
   )
   literals (
   )
))
