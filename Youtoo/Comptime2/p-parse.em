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
;;; Title: Parse module (defined in i-param)
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    ME: Module expander (import, top-lexical bindings)
;;    SE: Syntax expander (syntax-1)
;;    EE: Export expander (export)
;;    XE: Expression expander (expressions)
;;;-----------------------------------------------------------------------------

(defmodule p-parse
  (syntax (_syntax-1)
   import (i-all
           p-env
           ex-module
           ex-body
           sx-write
           sx-node)
   export (interactive-parse))

(defmethod parse-module (sexprs)
  (notify "  Parsing module ~a ..." *tmp-source-file-name*)
  (setq *pass* 'parse)
  (notify0 " .ME/SE")
  (if (and (cons? sexprs)
           (eq (car sexprs) 'defmodule))
      (let ((module (expand-module sexprs)))
        (dynamic-let  ((*actual-module* module))
                      (notify0 " .EE")
                      (expand-export module)
                      (notify0 " .XE/SE")
                      (expand-bodies module)
                      module))
    (ct-serious-warning
     () "bad defmodule syntax in module ~a ..." *tmp-source-file-name*)))

(defun interactive-parse (sexpr)
  (let ((module (dynamic *actual-module*)))
    (setq *pass* 'parse)
    (notify0 " .ME/SE")
    (expand-module sexpr)
    (notify0 " .XE/SE")
    (expand-bodies module)
    module))

;;;-----------------------------------------------------------------------------
)  ;; End of module p-parse
;;;-----------------------------------------------------------------------------
