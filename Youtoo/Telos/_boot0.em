;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: boot macros
;;;-----------------------------------------------------------------------------
(defmodule _boot0
  (import (level1))

;;;-----------------------------------------------------------------------------
;;; Control syntax
;;;-----------------------------------------------------------------------------
(defmacro cond body
  (if body
      (if (cdr (car body))
          `(if ,(car (car body))
               (progn ,@(cdr (car body)))
             (cond ,@(cdr body)))
        `(or ,(car (car body)) (cond ,@(cdr body))))
    ()))

(defmacro and body
  (if body
      (if (cdr body)
          `(if ,(car body)
               (and ,@(cdr body))
             ())
        (car body))
    t))

(defmacro or body
  (if body
      (if (cdr body)
          (let ((x (gensym)))
            `(let ((,x ,(car body)))
               (if ,x
                   ,x
                 (or ,@(cdr body)))))
        (car body))
    ()))

(defmacro when (pred . body) `(if ,pred (progn ,@body) ()))

(defmacro unless (pred . body) `(if ,pred () (progn ,@body)))

;;;-----------------------------------------------------------------------------
;;; Global register access
;;;-----------------------------------------------------------------------------
(defmacro set-global-register (name value)
  `((opencoded-lambda (x)
                      (set-register-ref ,name)
                      (register-ref ,name)) ,value))

(defmacro get-global-register (name)
  `((opencoded-lambda ()
                      (register-ref ,name))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
