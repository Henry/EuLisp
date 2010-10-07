;;; Verbose META in EuLisp.
;;;-----------------------------------------------------------------------------
;;; Support syntax-0 for VMETA
;;;  Author: T. Kurt Bond
;;;-----------------------------------------------------------------------------
(defmodule vmeta-aux
  (syntax (syntax-0)
   import (level-0))

(defmacro ecase (exp . clauses)
  (let ((val (gensym)))
    `(let ((,val ,exp))
       (cond
         ,@(map (lambda (clause)
                  (let ((const (car clause))
                        (forms (cdr clause)))
                    `((eq ,val ',const) ,@forms)))
                clauses)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
