;;; level0.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule level0
    (import
     ((rename
        ((begin progn)
         (pair? cons?)
         (real? float?)
         (real? double-float?)
         (procedure? function?)
         (eq? eq)
         (eqv? eql))
        (except
          (defmodule)
          root
          system
          thread
          telos
          telosint
          setter
          convert
          condcl
          arith
          compare
          macros
          collect
          copy
          format)))
     export
     (;; specials
      deflocal
      defconstant
      quote
      lambda
      delay
      let
      let*
      setq
      if
      cond
      progn
      and
      or
      while
      export
      expose
      enter-module
      !>
      reenter-module
      !>>
      call-next-method
      next-method?
      defclass
      apply
      map-list
      load
      load-noisily
      force

      ;; list functions
      cons
      car
      cdr
      caar
      cadr
      cdar
      cddr
      caaar
      caadr
      cadar
      caddr
      cdaar
      cdadr
      cddar
      cdddr
      caaaar
      caaadr
      caadar
      caaddr
      cadaar
      cadadr
      caddar
      cadddr
      cdaaar
      cdaadr
      cdadar
      cdaddr
      cddaar
      cddadr
      cdddar
      cddddr
      list
      list*
      append
      last-pair
      length
      memv
      memq
      assv
      assq
      list-ref
      list-tail

      ;; symbol functions
      bound?
      symbol-value
      symbol-plist
      gensym
      get
      put

      ;; vector functions
      vector
      make-vector
      vector-length
      vector-ref

      ;; array functions
      make-array
      array-ref

      ;; predicates
      null?
      atom
      list?
      number?
      boolean?
      cons?
      symbol?
      keyword?
      complex?
      float?
      double-float?
      rational?
      integer?
      char?
      string?
      vector?
      function?
      stream?
      input-stream?
      output-stream?
      object?
      eof-object?
      default-object?
      eq
      eql

      ;; arithmetic functions
      zero?
      positive?
      negative?
      odd?
      even?
      exact?
      inexact?
      truncate
      floor
      ceiling
      round
      abs
      gcd
      lcm
      random
      +
      -
      *
      /
      quotient
      remainder
      min
      max
      sin
      cos
      tan
      asin
      acos
      atan
      exp
      sqrt
      pow
      log
      binary+
      binary-
      unary-
      binary*
      binary/
      unary/
      binary%
      binary-gcd

      ;; bitwise logical functions
      logand
      logior
      logxor
      lognot

      ;; numeric comparison functions
      <
      <=
      =
      >=
      >

      ;; string functions
      make-string
      string-length
      string-null?
      string-append
      string-ref
      substring

      ;; i/o functions
      read
      read-char
      read-byte
      read-short
      read-long

      swrite
      write
      write-char
      write-byte
      write-short
      write-long

      sprin
      sprint
      prin
      print

      snewline
      newline

      char-ready?
      peek-char

      ;; print control functions
      print-breadth
      print-depth

      ;; file i/o functions
      open-input-file
      open-output-file
      open-append-file
      open-update-file
      close-stream
      close-input-stream
      close-output-stream
      get-file-position
      unlink

      ;; Standard streams
      stdin
      stdout
      stderr

      ; utility functions
      transcript-on
      transcript-off
      getarg
      prompt?
      load-module?
      exit
      compile
      decompile
      gc
      save
      restore

      ;; debugging functions
      trace-on
      trace-off

      ;; module functions
      module-symbols
      module-exports
      symbol-module
      current-module
      module-list
      unintern

      ;; telos
      allocate
      describe
      class?
      subclass?

      ;; tables
      make-table
      table-ref
      table-comparator
      table-delete
      table-length
      table-keys
      table-values
      table-fill
      table-clear

      ;; plus some others
      binary
      text
      not
      prin1
      princ
      t
      eval                             ; no guarantees this one will work
      system
      getenv
      putenv
      tmpfile
      current-time
      ticks-per-second
      backtrace
      backtrace?

      ;; thread
      <thread>
      <simple-thread>
      make-thread
      thread?
      thread-reschedule
      current-thread
      thread-kill
      thread-queue
      current-thread
      thread-start
      thread-value
      thread-state
      <thread-condition>
      <thread-error>
      <thread-already-started>

      <lock>
      <simple-lock>
      make-lock
      lock?
      lock
      unlock
      <lock-condition>
      <lock-error>

      wait
      <wait-condition>
      <wait-error>

      let/cc
      with-handler
      unwind-protect
      <wrong-condition-class>
      signal
      error
      cerror

      ;; telos
      <object>
      <class>
      <simple-class>
      <list>
      <cons>
      <null>
      <number>
      <integer>
      <fpi>
      <float>
      <double-float>
      <symbol>
      <keyword>
      <string>
      <simple-string>
      <stream>
      <input-stream>
      <output-stream>
      <i/o-stream>
      <vector>
      <simple-vector>
      <char>
      <simple-char>
      <promise>
      <table>
      <hash-table>
      <function>
      <simple-function>
      <subr>
      <continuation>
      <generic>
      <simple-generic>
      <method>
      <simple-method>
      <slot>
      <local-slot>
      <structure>

      generic-prin
      generic-write
      wait

      make
      initialize
      class-hierarchy

      ;; setter
      setter

      ;; converter
      converter

      convert
      <conversion-condition>
      <no-converter>

      ;; condcl
      defcondition
      condition?
      condition-message
      condition-value
      <condition>
      <telos-condition>
      <telos-error>
      <telos-general-error>
      <telos-bad-ref>
      <no-applicable-method>
      <no-next-method>
      <incompatible-method-domain>
      <arithmetic-condition>
      <arithmetic-error>
      <error>
      <general-error>
      <bad-type>
      <unbound-error>
      <compilation-error>
      <macro-error>
      <syntax-error>
      <user-interrupt>

      ;; compare
      binary<
      binary=
      <
      =
      >
      <=
      >=
      max
      min
      assoc

      ;; macros
      defmacro
      quasiquote
      unquote
      unquote-splicing
      symbol-macro
      macroexpand
      macroexpand1
      ;   syntax
      ;   dprint

      ;; collect
      <collection-condition>
      <collection-error>
      collection?
      sequence?
      accumulate
      accumulate1
      all?
      any?
      concatenate
      delete
      do
      element
      empty?
      fill
      map
      member
      remove
      reverse
      size
      slice

      ;; copy
      deep-copy
      shallow-copy

      ;; format
      format
      )
     export
     (;; from telosint, export them all while developing
      class-of
      class-name
      class-superclasses
      class-precedence-list
      class-slots
      class-keywords
      class-subclasses
      class-instance-size
      class-abstract?
      generic-name
      generic-args
      generic-optargs?
      generic-methods
      generic-cache1
      generic-cache2
      method-generic
      method-function
      method-domain
      add-method
      slot-name
      slot-keyword
      slot-default
      slot-required?
      )
     export
     (block
      return-from
      labels
      when
      unless
      while
      defun
      defgeneric
      defmethod
      generic-lambda
      method-lambda
      import
      syntax
      defmodule))

  (defmacro block (tag . body)
    (if (symbol? tag)
        `(let/cc ,tag ,@body)
      (error "not a symbol in block"
             <compilation-general-error>
             value: tag)))

  (defmacro return-from (tag . val)
    (if (symbol? tag)
        (if (null? val)
            `(,tag ())
          `(,tag ,@val))
      (error "not a symbol in return-from"
             <compilation-general-error>
             value: tag)))

  (define (letrec-binding binding)
          (list
            (car binding)
            (cons 'lambda (cdr binding))))

  (defmacro labels (bindings . body)
    `(letrec
       ,(map-list letrec-binding bindings)
       ,@body))

  (defmacro when (test . body)
    `(if ,test (progn ,@body) ()))

  (defmacro unless (test . body)
    `(if ,test () (progn ,@body)))

  (defmacro while (test . body)
    `(let/cc {break}                    ; break can be captured in body
       (letrec
         ((loop (lambda ()
                  (when ,test
                        ,@body
                        (loop)))))
         (loop))))

  (define (definable-name? name)
          (and (cons? name)
               (or (eq (car name) 'setter)
                   (eq (car name) 'converter))))

  ; (defun foo (x) ...)
  ; (defun (setter foo) (x) ...)
  ; (defun (converter foo) (x) ...)
  (defmacro defun (name args . body)
    (cond ((symbol? name)
           (if (bound? name)
               (progn
                 (prin "*** redefining ")
                 (prin name)
                 (prin " in module ")
                 (print (current-module))))
           `(define ,(cons name args)
                    ,@body))
          ((definable-name? name)
           `(progn
              ((setter ,(car name)) ,(cadr name)
               (lambda ,args ,@body))
              ',name))
          (t (error "malformed name in defun"
                    <compilation-general-error>
                    value: name))))

  ; (defgeneric foo (x)
  ;    method: ((x <int>) ...)
  ;    method: ((y <flt>) ...)
  ;    ...)
  (defmacro defgeneric (name args . body)
    (cond ((symbol? name)
           `(progn (define-generic ,(cons name args))
                   ,@(defgeneric-methods name body)
                   ',name))
          ((definable-name? name)
           `(progn
              (define-generic ,(cons 'setter/converter args))
              ((setter ,(car name)) ,(cadr name) setter/converter)
              ,@(defgeneric-methods
                  (list 'setter (cadr name))
                  body)
              ',name))
          (t (error "malformed name in defgeneric"
                    <compilation-general-error>
                    value: name))))

  (define (defgeneric-methods name body)
          (cond ((null? body) ())
                ((not (eq (car body) method:))
                 (error "unknown keyword in defgeneric"
                        <compilation-general-error>
                        value: (car body)))
                ((null? (cdr body))
                 (error "odd-length keyword list in defgeneric"
                        <compilation-general-error>
                        value: name))
                (t (cons
                     `(defmethod ,name ,(caadr body) ,@(cdadr body))
                     (defgeneric-methods name (cdr (cdr body)))))))

  (defmacro defmethod (name args . body)
    (if (or (symbol? name)
            (definable-name? name))
        `(define-method ,(cons name args) ,@body)
      (error "malformed name in defgeneric"
             <compilation-general-error>
             value: name)))

  (defmacro generic-lambda (args . body)
    `(let (anonymous-generic)
       (define-generic (anonymous-generic ,@args))
       ,@(defgeneric-methods
           'anonymous-generic
           body)
       anonymous-generic))

  (defmacro method-lambda (args . body)
    `(lambda (next-methods arg-list ,@args)
       ,@body))

  (defmacro import (mod)
    (if (not (or (string? mod)
                 (symbol? mod)))
        (error "bad module name in import"
               <compilation-general-error>
               value: mod)
      `(progn
         (setq curmod (find-module (current-module)))
         (%IMPORT curmod ,mod))))

  (defmacro syntax (mod)
    (if (not (or (string? mod)
                 (symbol? mod)))
        (error "bad module name in syntax"
               <compilation-general-error>
               value: mod)
      `(progn
         (setq curmod (find-module (current-module)))
         (%IMPORT curmod ,mod))))

  (defmacro defmodule (name . body)
    (error "only use defmodule in root module"
           <compilation-general-error>
           value: name))
  )
