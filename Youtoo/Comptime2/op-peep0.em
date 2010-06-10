;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: eval (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Keith Playford, Andreas Kind
;;; Description: peep-hole optimization rules
;;;-----------------------------------------------------------------------------
(defmodule op-peep0
  (syntax (macros)
   import (level1))

;;;-----------------------------------------------------------------------------
;;; Macros to define peep-hole optimization rules
;;;-----------------------------------------------------------------------------
  (defmacro guarded-rule (lhs guard rhs)
    (labels
     ((rule-parameters (lhs)
                       (if (null? lhs) ()
                         (append!
                          (map
                           (lambda (param)
                             (if (or (null? (symbolp param)) (eq param '*))
                                 '*no-variable*
                               param))
                           (cdr (car lhs)))
                          (rule-parameters (cdr lhs))))))
     `(add-rule ',lhs
                ,(if guard
                     `(lambda ,(rule-parameters lhs) ,guard)
                   ())
                (lambda ,(rule-parameters lhs)
                  (list ,@(map
                           (lambda (op)
                             `(list ',(car op)
                                    ,@(cdr op)))
                           rhs)))
                ,(size lhs))))

  (defmacro simple-rule (lhs rhs)
    `(guarded-rule ,lhs () ,rhs))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
