;;; Copyright 2010 Henry G. Weller and Stefan Israelsson Tampe
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
;;; Title: Macros for expanding smatch
;;;  Library: smatch
;;;  Authors: Henry G. Weller and Stefan Israelsson Tampe
;;;  Maintainers: Henry G. Weller and Stefan Israelsson Tampe
;;;  Description:
;;    See expand-smatch.em
;;;-----------------------------------------------------------------------------

(defmodule expand-match-syntax
  (syntax (syntax-0)
   import (level-0
           eval))

(defun expand-syntax-module-file (iname oname)
  (let* ((module (with-input-file (f iname)
                                  (read-s-expression f)))
         (specs (caddr module))
         (defs (cdddr module)))
    (letfuns ((proc (def)
                    (when (and (eq (car def) 'defsyntax)
                               (eq (car (cadddr def)) 'smatch0))
                          ((setter cadddr) def (expand-syntax (cadddr def)))))
              (loop (defs)
                    (when defs
                          (proc (car defs))
                          (loop (cdr defs)))))
      (loop defs)
      (with-output-file (f oname)
                        (swrite f module)))))

(expand-syntax-module-file "smatch-smatch0.em" "smatch.em")

;;;-----------------------------------------------------------------------------
)  ;; End of module expand-match-syntax
;;;-----------------------------------------------------------------------------
