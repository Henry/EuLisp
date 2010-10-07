;; Extra syntax-0
(defmodule extras0
  (syntax (syntax-0)
   import (level-0))

(defmacro and-let* (vars . body)
  (labels ((expand (vars body)
                   (cond
                     ((null? vars)
                      `(progn ,@body))
                     ((cons? vars)
                      (let ((exp (car vars)))
                        (cond
                          ((cons? exp)
                           (cond
                             ((null? (cdr exp))
                              `(and ,(car exp) ,(expand (cdr vars) body)))
                             (t
                              (let ((var (car exp))
                                    (val (cadr exp)))
                                `(let (,exp)
                                   (and ,var ,(expand (cdr vars) body)))))))
                          (t
                           `(and ,exp ,(expand (cdr vars) body))))))
                     (t
                      (error <condition> (fmt "not a proper list~a~%" vars))))))
          (expand vars body)))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
