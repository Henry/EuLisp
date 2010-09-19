;;; EuLysses header
(defmodule ops-ak
  (syntax (macros macros-tag)
   import (level1 basic cond-el-gf wm conflict))
;  (syntax (macros)
;   import (level1 basic cond-el-gf prod-gf wm-gf ops5-def ;ops-out
;          )
;   export ())

(print 'OK)

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
(print 'OK)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
