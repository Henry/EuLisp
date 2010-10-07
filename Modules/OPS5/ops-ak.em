;;; EuLysses header
(defmodule ops-ak
  (syntax (syntax-0 macros-tag)
   import (level-0 basic cond-el-gf wm conflict))
;  (syntax (syntax-0)
;   import (level-0 basic cond-el-gf prod-gf wm-gf ops5-def ;ops-out
;          )
;   export ())

(print 'OK nl)

(defclass <ops5-system> ()
  ((ce-manager
    reader:  ce-manager
    writer:  set-ce-manager)
   (cr-manager
    default: (make-cr-manager 'mea)
    reader:  cr-manager
    writer:  set-cr-manager)
   (wm-manager
    default: (make-wm-manager)
    reader:  wm-manager
    writer:  set-wm-manager)))
(print 'OK nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
