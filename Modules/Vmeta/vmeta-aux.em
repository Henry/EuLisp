;;; Verbose META in EuLisp.
;;;-----------------------------------------------------------------------------
;;; Support macros for VMETA
;;;  Author: T. Kurt Bond
;;;-----------------------------------------------------------------------------
(defmodule vmeta-aux
  (syntax (macros)
   import (level1))

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
  )  ;; end of module
;;;-----------------------------------------------------------------------------
