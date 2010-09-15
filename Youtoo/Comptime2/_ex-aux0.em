;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: syntactic sugar helpful in the expansion phase
;;;-----------------------------------------------------------------------------

(defmodule _ex-aux0
  (syntax (macros)
   import (level1))

(defmacro get-name (form)
  `(let ((x (cadr ,form)))
     (if (symbol? x)
         x
       (if (and (cons? x) (eq (car x) 'setter))
           x
         (error <condition>
                (fmt "bad value ~a" x))))))

(defmacro get-params (form) `(caddr ,form))

(defmacro get-lambda-params (form) `(cadr ,form))

(defmacro get-body (form)
  `(let ((x (cdr (cddr ,form))))
     (if (or (null? x) (cons? x))
         x
       (error <condition>
              (fmt "body ~a not a list" x)))))

(defmacro get-lambda-body (form)
  `(let ((x (cddr ,form)))
     (if (or (null? x) (cons? x))
         x
       (error <condition>
              (fmt "body ~a not a list" x)))))

(defmacro get-value (form) `(caddr ,form))

(defmacro get-directives (form) `(caddr ,form))

(defmacro get-top-level-forms (form) `(cdr (cddr ,form)))

;;;-----------------------------------------------------------------------------
)  ;; End of module ex-aux0
;;;-----------------------------------------------------------------------------
