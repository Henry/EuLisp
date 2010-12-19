;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: peep-hole optimization rules
;;;  Library: eval (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Keith Playford, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule _op-peep0
  (syntax (syntax-1)
   import (level-1))

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
                           (if (or (null? (symbol? param)) (eq param '*))
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
)  ;; End of module op-peep0
;;;-----------------------------------------------------------------------------
