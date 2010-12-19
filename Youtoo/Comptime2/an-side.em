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
;;; Title: side effect analyses
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule an-side
  (syntax (_syntax-1)
   import (i-all
           sx-obj)
   export (compute-captured-vars))

;;;-----------------------------------------------------------------------------
;;; Set a flag when a local-static-var is once captured (free in a lambda)
;;;-----------------------------------------------------------------------------
(defgeneric compute-captured-vars (node))

(defmethod compute-captured-vars ((node <module>))
  (do1-list compute-captured-vars (module-named-lambdas? node))
  (do1-list compute-captured-vars (module-anonymous-lambdas? node)))

(defmethod compute-captured-vars ((node <lambda>))
  (let* ((args (append (fun-args? node) (lambda-delegated-vars? node)))
         (bindings (lambda-binding-refs? node))
         (objs (map1-list binding-obj? bindings))
         (vars (select-list local-static-var? objs)))
    (do1-list (lambda (var)
                (and (null? (member1-list var args))
                     (progn
                       (notify0 "var ~a captured" (var-name? var))
                       (local-static-var-captured! var t))))
              vars)))

;;;-----------------------------------------------------------------------------
)  ;; End of module an-side
;;;-----------------------------------------------------------------------------
