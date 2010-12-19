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
;;; Title: root module of the compiler
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule main
  (syntax (_syntax-1
           _i-aux0)
   import (i-all
           i-args
           i-compile
           i-modify
           cg-interf
           i-rep
           eval)
   export (main
           eval
           rep ?
           dynamic-binding-ref
           dynamic-binding-set
           dynamic-load-module
           as-dynamic-binding
           module-loaded?
           as-C-module-name
           dynamic-binding-ref1
           dynamic-binding-set1
           dynamic-load-module1
           *redefine-imported-bindings*
           macroexpand-1
           macroexpand
           expand-syntax-1
           expand-syntax))

;;;-----------------------------------------------------------------------------
;;; Main function to invoke the interpreter or compiler
;;;-----------------------------------------------------------------------------
(defun main (argv)
  ;; Error handler
  (with-handler (generic-lambda (c k)
                                method: ((c k)) ; pass signal to next handler
                                method: (((c <ct-error>) k)
                                         (sprint stderr c nl)
                                         (ct-exit -1)))
                (ct-reset)
                (parse-args argv)
                (check-stop)
                (if *script* ()
                  ;; Don't want interleaving messages
                  (setq stderr stdout))
                (start-eulysses))
  (sflush stdout)
  (sflush stderr))

;;;-----------------------------------------------------------------------------
;;; Check if compiler or interpreter should be invoked
;;;-----------------------------------------------------------------------------
(defun start-eulysses ()
  (notify "")  ; newline
  (notify0 "Start EuLysses ...")
  (cond (*interpreter* ;; Invoke interpreter
         (progn
           (rep)
           (exit 0)))
        (*eval-module* ;; Evaluate the specified module
         (dynamic-load-module (make <symbol> name: *script-file*) t)
         (exit 0))
        (t ;; Invoke compiler
         (progn
           (load-library-interfaces)
           (do1-list
            (lambda (module-name)
              (setq *tmp-start-source-file-name* module-name)
              ;; Check if recompilation is necessary
              (if (directly-or-indirectly-modified? module-name)
                  (compile module-name)
                (notify "Module ~a need not be recompiled." module-name))
              (link module-name))
            *source-file-names*)
           (ct-exit)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module main
;;;-----------------------------------------------------------------------------
