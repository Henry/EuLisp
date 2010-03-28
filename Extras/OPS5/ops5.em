;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;                 OPS5 for EuLisp System 'youtoo'
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; File   : ops5.em
;;; Date   : 20 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Desc.  : main module
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defmodule ops5
;;; Uncomment this block to run under youtoo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; BEGIN_YOUTOO
(syntax (macros macros-tag) 
 import (level1 basic conflict cond-el-gf wm reader ops5-def)
 expose (conflict cond-el-gf wm reader ops5-def)
 export (ops-load))
;(let/cc exit 
;(let loop ((x ())) 
;(format t "Enter name of OPS5 file: ") 
;(let/cc restart 
;(with-handler 
;(lambda (c k) 
;(output-condition-contents c) 
;(restart nil)) 
;(setq x (read lispin () (eos-default-value))) 
;(if (eq x (eos-default-value)) 
;(progn (print "Exiting") (exit 0)) 
;(ops-load x)))) 
;(loop x))) 
;;; END_YOUTOO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uncomment this block to run under euscheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN_EUSCHEME
;;  (import (level0 conflict reader ops5-def wm cond-el-gf conflict))
;;; END_EUSCHEME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (print "### ops5")
  (defun ops-load (filename)
    (let ((reader (read-ops-prog (symbol-name filename)))
          (ops-sys (make <ops5-system>)))
      (print-ces (ce-man reader))
      (set-ce-manager ops-sys (ce-man reader))
      (print "OPS5: Network has been initialised")
      (insert-wme (wm-manager ops-sys) (ce-manager ops-sys) 
                  (cr-manager ops-sys) 'start ())
      (print "OPS5: Start working memory element inserted")
      (fire-prod-inst (cr-manager ops-sys) (wm-manager ops-sys) 
                      (ce-manager ops-sys))))
) ;; module: ops5
