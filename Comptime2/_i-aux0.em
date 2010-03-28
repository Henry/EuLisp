;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Description: error handler and file name extensions
;;; -----------------------------------------------------------------------
(defmodule _i-aux0
  (syntax (macros)
   import (level1))
;;; ---------------------------------------------------------------------
;;; Marcro to invoke the compiler
;;; ---------------------------------------------------------------------
  (defmacro youtoo args
    `(main (cons 'youtoo ',args)))
;;; --------------------------------------------------------------------
;;; Compile-time error handler wrapper
;;; --------------------------------------------------------------------
  (defmacro with-ct-handler (str error-value . forms)
    `(with-handler
      (generic-lambda (c f)
        method: ((c f)
                 (format stderr "compile time error condition: ")
                 (pprint c stderr)
                 (if *no-ct-handlers* ()
                   (error ,str <ct-error> ct-error-value: ,error-value)))
        method: (((c <ct-error>) f)))   ; pass signal to next handler
      (progn ,@forms)))
;;; --------------------------------------------------------------------
;;; File name extensions
;;; --------------------------------------------------------------------
  (defmacro as-source-file-name (name)
    `(string-append (or (stringp ,name) (symbol-name ,name)) ".em"))
  (defmacro as-interface-file-name (name)
    `(string-append (or (stringp ,name) (symbol-name ,name)) ".i"))
  (defmacro as-C-file-name (name)
    `(string-append (or (stringp ,name) (symbol-name ,name)) ".c"))
  (defmacro as-compiled-C-file-name (name)
    `(if *object-dir*
         (string-append *object-dir*
           (string-append *delimiter*
             (string-append (or (stringp ,name) (symbol-name ,name)) ".o")))
       (string-append (or (stringp ,name) (symbol-name ,name)) ".o")))
  (defmacro as-included-C-file-name (name)
    `(string-append (or (stringp ,name) (symbol-name ,name)) ".h"))
  (defmacro as-C-hook-name (name)
    `(string-append (or (stringp ,name) (symbol-name ,name)) "_"))
  (defmacro as-C-hook-source-file-name (name)
    `(string-append (or (stringp ,name) (symbol-name ,name)) "_.c"))
  (defmacro as-C-hook-object-file-name (name)
    `(if *object-dir*
         (string-append *object-dir*
           (string-append *delimiter*
             (string-append (or (stringp ,name) (symbol-name ,name)) "_.o")))
       (string-append (or (stringp ,name) (symbol-name ,name)) "_.o")))
  (defmacro as-C-library-file-name (name)
    `(if *object-dir*
         (format () "~a~alib~a.a" *object-dir* *delimiter* ,name)
       (format () "lib~a.a" ,name)))
  (defmacro as-C-library-link-string (name)
    `(string-append " -l" (or (stringp ,name) (symbol-name ,name))))
  (defmacro as-C-library-dir-link-string (name)
    `(string-append " -L" (or (stringp ,name) (symbol-name ,name))))
  (defmacro vm-link-string () " -leulvm")
  (defmacro gc-link-string () '(if *no-gc* "" "-lgc"))
  (defmacro as-C-library-interface-file-name (name)
    `(format () "lib~a.i" ,name))
  (defmacro as-foreign-function-stub-name (name)
    `(string-append "ff_stub_" (symbol-name (gensym ,name))))
  (defmacro as-module-init-function-name (name)
    `(if *debug*
        (make-symbol
         (string-append (or (stringp ,name) (symbol-name ,name))
                        "-init-fun"))
      ()))  ; no lambda naming
  (defmacro as-module-init-flag-name (name)
    `(make-symbol
      (string-append (or (stringp ,name) (symbol-name ,name))
                     "-init-flag")))
  (defmacro full-C-library-link-string ()
    '(let ((str-list
            (map1-list
             (lambda (name) (as-C-library-link-string name))
             *linked-C-libraries*)))
      (if (null str-list) ""
        (apply concatenate str-list))))
  (defmacro full-C-library-dir-link-string ()
    '(let ((str-list
            (map1-list
             (lambda (name) (as-C-library-dir-link-string name))
             *C-library-load-path*)))
      (if (null str-list) ""
        (apply concatenate str-list))))
  (defmacro main-link-string ()
    '(let ((name (format () "Lib.~a/eul-appl.o" (get-config-info 'ARCH))))
       (format () "~a~a~a"
               *eulysses-dir* *delimiter* name)))
  (defmacro destination-link-string (module-name dir)
    `(format () "~a~a~a" ,dir *delimiter*
             (or *dest-file-name* ,module-name)))
  (defmacro destination-library-link-string (module-name dir)
    `(or *dest-file-name*
         (format () "~a~a~a" ,dir *delimiter*
                 (as-C-library-file-name ,module-name))))
  (defmacro destination-object-string (module-name dir)
    `(format () "~a~a~a" ,dir *delimiter*
             (or *dest-file-name*
                 (as-compiled-C-file-name ,module-name))))
)  ; end of module
