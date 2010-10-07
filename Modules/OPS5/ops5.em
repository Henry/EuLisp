;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : ops5.em
;;; Date   : 20 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: main module
;;;-----------------------------------------------------------------------------
(defmodule ops5
  (syntax (macros macros-tag)
   import (level1 basic conflict cond-el-gf wm reader ops5-def)
   expose (conflict cond-el-gf wm reader ops5-def)
   export (ops-load))

;(let/cc exit
;(let loop ((x ()))
;(format "Enter name of OPS5 file: ")
;(let/cc restart
;(with-handler
;(lambda (c k)
;(output-condition-contents c)
;(restart ()))
;(setq x (read lispin () (eos-default-value)))
;(if (eq x (eos-default-value))
;(progn (print "Exiting" nl) (exit 0))
;(ops-load x))))
;(loop x)))

(print "### ops5" nl)

(defun ops-load (filename)
  (let ((reader (read-ops-prog (symbol-name filename)))
        (ops-sys (make <ops5-system>)))
    (print-ces (ce-man reader))
    (set-ce-manager ops-sys (ce-man reader))
    (print "OPS5: Network has been initialised" nl)
    (insert-wme (wm-manager ops-sys) (ce-manager ops-sys)
                (cr-manager ops-sys) 'start ())
    (print "OPS5: Start working memory element inserted" nl)
    (fire-prod-inst (cr-manager ops-sys) (wm-manager ops-sys)
                    (ce-manager ops-sys))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
