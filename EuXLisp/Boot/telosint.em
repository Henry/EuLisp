;;; telosint.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford
;;;
;;; symbols not usually exported from telos.em
;;; mainly introspective stuff

(defmodule telosint

    (import (root))

  ; install <class> slot defaults
  (deflocal slots (class-slots <class>))

  (set-slot-default! (list-ref slots 0)
                     (lambda () 'anon)) ; name
  (set-slot-default! (list-ref slots 1)
                     (lambda () (list <structure>))) ; superclasses
  (set-slot-default! (list-ref slots 5)
                     (lambda () ()))    ; subclasses
  (set-slot-default! (list-ref slots 7)
                     (lambda () ()))    ; abstract?

  ; install <simple-class> slot defaults
  (deflocal sslots (class-slots <simple-class>))

  (set-slot-default! (list-ref sslots 0)
                     (lambda () 'anon)) ; name
  (set-slot-default! (list-ref sslots 1)
                     (lambda () (list <structure>))) ; superclasses
  (set-slot-default! (list-ref sslots 5)
                     (lambda () ()))    ; subclasses
  (set-slot-default! (list-ref sslots 7)
                     (lambda () ()))    ; abstract?

  ;; tweaks to allow make of a few built-in classes
  (deflocal builtin-make-table (make-table eq?))

  ;; cons
  (define (mkcons inits)
          (let ((car (find-key car: inits ()))
                (cdr (find-key cdr: inits ())))
            (cons car cdr)))

  (set-class-keywords! <cons> '(car: cdr:))
  (table-set! builtin-make-table <cons> mkcons)

  ;; null
  (define (mknull inits)
          ())

  (table-set! builtin-make-table <null> mknull)

  ;; fpi
  (define (mkfpi inits)
          (find-key value: inits 0))

  (set-class-keywords! <fpi> '(value:))
  (table-set! builtin-make-table <fpi> mkfpi)

  ;; double-float
  (define (mkdfloat inits)
          (find-key value: inits 0.0))

  (set-class-keywords! <double-float> '(value:))
  (table-set! builtin-make-table <double-float> mkdfloat)

  ;; symbol
  (define (mksymbol inits)
          (let ((name (find-key string: inits "")))
            (string->symbol name)))

  (set-class-keywords! <symbol> '(string:))
  (table-set! builtin-make-table <symbol> mksymbol)

  ;; keyword
  (define (mkkeyword inits)
          (let* ((name (find-key string: inits ""))
                 (len (string-length name)))
            (if (= len 0)
                :                             ; null keyword
              (string->symbol
                (if (eqv? (string-ref name (- len 1)) #\:)
                    name
                  (string-append name ":"))))))

  (set-class-keywords! <keyword> '(string:))
  (table-set! builtin-make-table <keyword> mkkeyword)

  ;; string
  (define (mkstring inits)
          (let ((size (find-key size: inits 0))
                (fill (find-key fill-value: inits #\space)))
            (make-string
              size
              (if (integer? fill)
                  (integer->char fill)
                fill))))

  (set-class-keywords! <string> '(size: fill-value:))
  (set-class-keywords! <simple-string> (class-keywords <string>))
  (table-set! builtin-make-table <simple-string> mkstring)

  ;; input-port
  (define (mkiport inits)
          (let ((filename (find-key filename: inits ())))
            (open-input-file filename)))

  (set-class-keywords! <input-port> '(filename:))
  (table-set! builtin-make-table <input-port> mkiport)

  ;; output-port
  (define (mkoport inits)
          (let ((filename (find-key filename: inits ())))
            (open-output-file filename)))

  (set-class-keywords! <output-port> '(filename:))
  (table-set! builtin-make-table <output-port> mkoport)

  ;; i/o port ?

  ;; vector
  (define (mkvector inits)
          (let ((size (find-key size: inits 0))
                (fill (find-key fill-value: inits ())))
            (make-vector size fill)))

  (set-class-keywords! <vector> '(size: fill-value:))
  (set-class-keywords! <simple-vector> (class-keywords <vector>))
  (table-set! builtin-make-table <simple-vector> mkvector)

  ;; char
  (define (mkchar inits)
          (let ((code (find-key code: inits 32)))
            (integer->char code)))

  (set-class-keywords! <char> '(code:))
  (set-class-keywords! <simple-char> (class-keywords <char>))
  (table-set! builtin-make-table <simple-char> mkchar)

  ;; promise, env, code, module, simple-function, closure,
  ;; subr, xsubr, csubr, continuation, slot
  (define (cant-make cl)
          (lambda (inits)
            (raise-telos-error "can't make instance of class" cl)))

  (table-set! builtin-make-table <promise> (cant-make <promise>))
  (table-set! builtin-make-table <env> (cant-make <env>))
  (table-set! builtin-make-table <code> (cant-make <code>))
  (table-set! builtin-make-table <module> (cant-make <module>))
  (table-set! builtin-make-table <simple-function>
              (cant-make <simple-function>))
  (table-set! builtin-make-table <closure> (cant-make <closure>))
  (table-set! builtin-make-table <subr> (cant-make <subr>))
  (table-set! builtin-make-table <xsubr> (cant-make <xsubr>))
  (table-set! builtin-make-table <csubr> (cant-make <csubr>))
  (table-set! builtin-make-table <continuation> (cant-make <continuation>))

  ;; slot
  (set-class-keywords! <slot> '(keyword: default: requiredp:))
  (set-class-keywords! <local-slot> (class-keywords <slot>))
  (table-set! builtin-make-table <local-slot> (cant-make <slot>))

  ;; table
  (define (mktable inits)
          (let ((comp (find-key comparator: inits eqv?))
                (fill (find-key fill-value: inits ())))
            (make-table comp fill)))

  (set-class-keywords! <table> '(comparator: fill-value:))
  (set-class-keywords! <hash-table> (class-keywords <table>))
  (table-set! builtin-make-table <hash-table> mktable)

  ;; for thread see thread.em

  ;; generic
  (define (mkgeneric inits)
          (let ((name (find-key name: inits 'anonymous-generic))
                (domain (find-key domain: inits ()))
                (optargs? (find-key optargs: inits ())))
            (if (null? domain)
                (raise-telos-error "missing keyword domain: in make <simple-generic>"
                                   inits))
            (make-generic name domain optargs?)))

  (set-class-keywords! <generic> '(name: domain: optargs:))
  (set-class-keywords! <simple-generic> (class-keywords <generic>))
  (table-set! builtin-make-table <simple-generic> mkgeneric)

  ;; method
  (define (mkmethod inits)
          (let ((fn (find-key method-lambda: inits ()))
                (domain (find-key domain: inits ()))
                (optargs? (find-key optargs: inits ())))
            (if (null? fn)
                (raise-telos-error
                  "missing keyword method-lambda: in make <simple-method>"
                  inits))
            (if (null? domain)
                (raise-telos-error "missing keyword domain: in make <simple-method>"
                                   inits))
            (make-method fn domain optargs?)))

  (set-class-keywords! <method> '(method-lambda: domain: optargs:))
  (set-class-keywords! <simple-method> (class-keywords <method>))
  (table-set! builtin-make-table <simple-method> mkmethod)

  (export next-methods arg-list)

  (define (strip<> name)
          (if (not (symbol? name))
              (raise-telos-error "class name not a symbol" name))
          (let* ((str (symbol->string name))
                 (len (string-length str)))
            (if (and (> len 1)
                     (eqv? (string-ref str 0) #\<)
                     (eqv? (string-ref str (- len 1)) #\>))
                (substring str 1 (- len 1))
              str)))

  (define (display-class-name obj port)
          (%display (strip<> (class-name (class-of obj))) port))

  (export

   class-of
   class-name
   class-superclasses
   class-precedence-list
   class-slots
   class-keywords
   set-class-keywords!
   class-subclasses
   class-instance-size
   class-abstract?
   class?
   subclass?
   generic-name
   generic-args
   set-generic-args!
   generic-optargs?
   generic-methods
   generic-cache1
   generic-cache2
   make-generic
   make-method
   method-generic
   method-function
   method-domain
   add-method
   slot-name
   slot-keyword
   slot-default
   set-slot-default!
   slot-required?
   set-slot-required?!
   find-slot-index

   initialize-object
   initialize-class
   builtin-make-table
   display-class-name

   )

  )
