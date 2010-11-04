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
;;; Title: error handler and file name extensions
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule _i-aux0
  (syntax (macros)
   import (level-1))

;;;-----------------------------------------------------------------------------
;;; Marcro to invoke the compiler
;;;-----------------------------------------------------------------------------
(defmacro youtoo args
  `(main (cons 'youtoo ',args)))

;;;-----------------------------------------------------------------------------
;;; Compile-time error handler wrapper
;;;-----------------------------------------------------------------------------
(defmacro with-ct-handler (str error-value . forms)
  `(with-handler
    (generic-lambda (c f)
     method: ((c f)
              (sformat stderr "compile time error condition: ")
              (spprint stderr c)
              (if *no-ct-handlers* ()
                (error <ct-error> ,str ct-error-value: ,error-value)))
     method: (((c <ct-error>) f))) ;; pass signal to next handler
    (progn ,@forms)))

;;;-----------------------------------------------------------------------------
;;; File name extensions
;;;-----------------------------------------------------------------------------
(defmacro as-source-file-name (name)
  `(string-append (or (string? ,name) (symbol-name ,name)) ".em"))

(defmacro as-interface-file-name (name)
  `(concatenate *u2-C-dir* *delimiter*
                (or (string? ,name) (symbol-name ,name)) ".i"))

(defmacro as-C-file-name (name)
  `(concatenate *u2-C-dir* *delimiter*
                (or (string? ,name) (symbol-name ,name)) ".c"))

(defmacro as-compiled-C-file-name (name)
  `(if *object-dir*
       (concatenate *object-dir* *delimiter* *u2-C-dir* *delimiter*
                    (or (string? ,name) (symbol-name ,name)) ".o")
     (string-append (or (string? ,name) (symbol-name ,name)) ".o")))

(defmacro as-compiled-fff-C-file-name (name)
  `(if *object-dir*
       (concatenate *object-dir* *delimiter*
                    (or (string? ,name) (symbol-name ,name)) ".o")
     (string-append (or (string? ,name) (symbol-name ,name)) ".o")))

(defmacro as-included-C-file-name (name)
  `(concatenate *u2-C-dir* *delimiter*
                (or (string? ,name) (symbol-name ,name)) ".h"))

(defmacro as-C-hook-name (name)
  `(string-append (or (string? ,name) (symbol-name ,name)) "_"))

(defmacro as-C-hook-source-file-name (name)
  `(concatenate *u2-C-dir* *delimiter*
                (or (string? ,name) (symbol-name ,name)) "_.c"))

(defmacro as-C-hook-object-file-name (name)
  `(if *object-dir*
       (concatenate *object-dir* *delimiter* *u2-C-dir* *delimiter*
                    (or (string? ,name) (symbol-name ,name)) "_.o")
     (string-append (or (string? ,name) (symbol-name ,name)) "_.o")))

(defmacro as-C-library-file-name (name)
  `(if *object-dir*
       (fmt "~a~alib~a.a" *object-dir* *delimiter* ,name)
     (fmt "lib~a.a" ,name)))

(defmacro as-C-library-link-string (name)
  `(string-append " -l" (or (string? ,name) (symbol-name ,name))))

(defmacro as-C-library-dir-link-string (name)
  `(string-append " -L" (or (string? ,name) (symbol-name ,name))))

(defmacro vm-link-string () " -leulvm")

(defmacro gc-link-string () '(if *no-gc* "" "-lgc"))

;; (defmacro as-C-library-interface-file-name (name)
;;   `(fmt "lib~a.i" ,name))

(defmacro as-C-library-interface-file-name (name)
  `(fmt "~a~alib~a.i" *u2-C-dir* *delimiter* ,name))

(defmacro as-foreign-function-stub-name (name)
  `(string-append "ff_stub_" (symbol-name (gensym ,name))))

(defmacro as-module-init-function-name (name)
  `(if *debug*
       (make-symbol
        (string-append (or (string? ,name) (symbol-name ,name))
                       "-init-fun"))
     ()))  ;; no lambda naming

(defmacro as-module-init-flag-name (name)
  `(make-symbol
    (string-append (or (string? ,name) (symbol-name ,name))
                   "-init-flag")))

(defmacro full-C-library-link-string ()
  '(let ((str-list
          (map1-list
           (lambda (name) (as-C-library-link-string name))
           *linked-C-libraries*)))
     (if (null? str-list) ""
       (apply concatenate str-list))))

(defmacro full-C-library-dir-link-string ()
  '(let ((str-list
          (map1-list
           (lambda (name) (as-C-library-dir-link-string name))
           *C-library-load-path*)))
     (if (null? str-list) ""
       (apply concatenate str-list))))

(defmacro main-link-string ()
  '(let ((name (fmt "Lib.~a/eul-appl.o" (get-config-info 'ARCH))))
     (fmt "~a~a~a" *eulysses-dir* *delimiter* name)))

(defmacro destination-link-string (module-name dir)
  `(fmt "~a~a~a" ,dir *delimiter*
        (or *dest-file-name* ,module-name)))

(defmacro destination-library-link-string (module-name dir)
  `(or *dest-file-name*
       (fmt "~a~a~a" ,dir *delimiter*
            (as-C-library-file-name ,module-name))))

(defmacro destination-object-string (module-name dir)
  `(fmt "~a~a~a" ,dir *delimiter*
        (or *dest-file-name*
            (as-compiled-C-file-name ,module-name))))

(defmacro destination-object-dir (dir)
  `(if *object-dir*
       (concatenate ,dir *delimiter* *object-dir* *delimiter* *u2-C-dir*)
     ,dir))

;;;-----------------------------------------------------------------------------
;;; Trace
;;; Actions are pre/post thunks with the traced function+parameters as
;;; arguments.
;;;-----------------------------------------------------------------------------
(defmacro trace (function-name . actions)
  (let* ((tmp-name (concatenate '| | function-name))
         (pre-action (if actions (car actions) ()))
         (post-action (if (and pre-action (cdr actions)) (cadr actions) ())))
    `(progn
       (deflocal ,tmp-name ())
       (setq *redefine-imported-bindings*
             (list *redefine-imported-bindings*))
       (setq ,tmp-name ,function-name)
       (setq ,function-name
             (named-lambda
              ,function-name args
              ,(if pre-action
                   `(apply ,pre-action ,function-name args)
                 `(sformat stderr
                           ,(fmt ">>> ~~aTRACE [~a]: ~~a\n" function-name)
                           (dynamic *trace-indent*) args))
              (let ((res (dynamic-let
                          ((*trace-indent*
                            (concatenate (dynamic *trace-indent*)
                                         " ")))
                          (apply ,tmp-name args))))
                ,(if post-action
                     `(apply ,post-action ,function-name args)
                   `(sformat stderr
                             ,(fmt "<<< ~~aTRACE [~a]: ~~a => ~~a\n"
                                   function-name)
                             (dynamic *trace-indent*) args res))
                res)))
       ;; retrieve previous value
       (setq *redefine-imported-bindings*
             (car *redefine-imported-bindings*))
       ,function-name)))

(defmacro untrace (function-name)
  (let ((tmp-name (concatenate '| | function-name)))
    `(progn
       (setq *redefine-imported-bindings*
             (list *redefine-imported-bindings*))
       (setq ,function-name ,tmp-name)
       ;; retrieve previous value
       (setq *redefine-imported-bindings*
             (car *redefine-imported-bindings*)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module i-aux0
;;;-----------------------------------------------------------------------------
