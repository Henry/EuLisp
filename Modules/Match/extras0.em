;; Extra macros
(defmodule extras0
  (syntax (macros)
   import (level1))

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
                      (error "not a proper list" vars)))))
          (expand vars body)))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
