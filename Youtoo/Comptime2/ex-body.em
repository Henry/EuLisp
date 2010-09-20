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
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: expand bodies of defining and top-level forms
;;;-----------------------------------------------------------------------------
(defmodule ex-body
  (syntax (_macros _sx-obj0)
   import (i-all p-env ex-expr sx-obj sx-node)
   export (expand-bodies))

;;;-----------------------------------------------------------------------------
;;; Expander for all collected simple expressions
;;;-----------------------------------------------------------------------------
(defun expand-bodies (module)
  (let ((top-level-lambda (set-up-top-level-lambda)))
    (notify0 "   Complete variable nodes")
    (do1-list complete-variable-node (module-static-variables? module))
    (notify0 "   Complete constant nodes")
    (do1-list complete-constant-node (module-named-constants? module))
    (notify0 "   Complete defun bodies")
    (do1-list complete-lambda-node (module-named-lambdas? module))
    (notify0 "   Expand top-level forms")
    (complete-top-level-forms module top-level-lambda)))

;;;-----------------------------------------------------------------------------
;;; Complete VARIABLE and CONSTANT nodes as top-level SETQs
;;;-----------------------------------------------------------------------------
(defun complete-variable-node (var)
  (new-node `(setq ,(var-name? var) ,(var-value? var))
            'top-level-form t)
  (var-value! var *nil*))

(defun complete-constant-node (const)
  (new-node `(setq ,(named-const-name? const) ,(const-value? const) t)
            'top-level-form t)
  (const-value! const *nil*))

;;;-----------------------------------------------------------------------------
;;; Complete TOP-LEVEL FORMS nodes
;;;-----------------------------------------------------------------------------
(defun set-up-top-level-lambda ()
  (let ((top-level-lambda (make-fun <lambda> 'top-level () ())))
    (dynamic-setq *encl-lambda* top-level-lambda)
    top-level-lambda))

(defun complete-top-level-forms (module top-level-lambda)
  (let ((exprs (module-top-level-forms? module))
        (appl (make <appl> fun: top-level-lambda args: ())))
    (notify0 "    Top-level forms: ~a" exprs)
    (fun-body! top-level-lambda `(progn ,@exprs))
    (complete-lambda-node top-level-lambda)
    (module-top-level-forms! module appl)))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
