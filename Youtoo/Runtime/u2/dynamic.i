;;; EuLisp system 'youtoo'
;;;   Interface file for module dynamic

(definterface dynamic
  (import (telos thread)
   syntax (_telos0)
   full-import (mop-alloc mop-access mop-prim mop-key mop-class mop-init mop-inspect mop-gf mop-meth mop-defcl boot boot1 telos event thread)
   export (
    ((name . pop-error-handlers) (pos . 6) (origin dynamic . pop-error-handlers))
    ((name . push-error-handler) (pos . 5) (origin dynamic . push-error-handler))
    ((name . *current-no-dynamic-variables*) (pos . 7) (origin dynamic . *current-no-dynamic-variables*))
    ((name . *current-no-error-handlers*) (pos . 4) (origin dynamic . *current-no-error-handlers*))
    ((name . dynamic-variable-ref) (pos . 3) (origin dynamic . dynamic-variable-ref))
    ((name . push-dynamic-variable) (pos . 2) (origin dynamic . push-dynamic-variable))
    ((name . pop-dynamic-variables) (pos . 8) (origin dynamic . pop-dynamic-variables))
   )
   local-literals (
    (top-level . 29)
    (push-dynamic-variable . 28)
    (dynamic-variable-ref . 27)
    (push-error-handler . 26)
    (pop-error-handlers . 25)
    (pop-dynamic-variables . 24)
    ("dynamic variable ~a not available" . 16)
    (|(setter dynamic-variable-ref)| . 13)
    (anonymous . 11)
    ("dynamic variable ~a not available" . 9)
   )
   literals (
   )
))
