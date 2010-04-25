;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: syntactic sugar helpful in the expansion phase
;;;-----------------------------------------------------------------------------
(defmodule ex-aux0
  (syntax (macros)
   import (level1))

  (defmacro get-name (form)
    `(let ((x (cadr ,form)))
       (if (symbolp x)
           x
         (if (and (consp x) (eq (car x) 'setter))
             x
           (error "bad value ~a" x)))))

  (defmacro get-params (form) `(caddr ,form))

  (defmacro get-lambda-params (form) `(cadr ,form))

  (defmacro get-body (form)
    `(let ((x (cdr (cddr ,form))))
       (if (or (null x) (consp x))
           x
         (error "body ~a not a list" x))))

  (defmacro get-lambda-body (form)
    `(let ((x (cddr ,form)))
       (if (or (null x) (consp x))
           x
         (error "body ~a not a list" x))))

  (defmacro get-value (form) `(caddr ,form))

  (defmacro get-directives (form) `(caddr ,form))

  (defmacro get-top-level-forms (form) `(cdr (cddr ,form)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
