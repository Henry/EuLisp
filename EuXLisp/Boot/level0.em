;;; Copyright (c) 1994 Russell Bradford
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;;  File: level0.em
;;;  Library: level0
;;;  Description: list processing
;;;-----------------------------------------------------------------------------

(defmodule level0
    (import
     (;; all of the level0 modules
      root
      macros
      macros0
      system
      thread
      telos
      telosint
      setter
      convert
      condcl
      arith
      compare
      collect
      copy
      format)

     export
     (;; Special operators
      deflocal
      defconstant
      defclass
      defun
      defgeneric
      defmethod
      quote
      lambda
      progn
      let
      let*
      setq
      if
      cond
      and
      or
      when
      unless
      while
      apply
      block
      return-from
      labels
      generic-lambda
      method-lambda

      ;; List functions
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
      append
      length
      list-ref
      member-alist

      ;; Symbol functions
      symbol-exists?
      symbol-value
      symbol-plist
      gensym
      get
      put

      ;; Vector functions
      vector
      vector-size
      vector-ref

      ;; Predicates
      null?
      atom
      list?
      number?
      cons?
      symbol?
      keyword?
      float?
      double-float?
      integer?
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

      ;; Arithmetic functions
      zero?
      positive?
      negative?
      odd?
      even?
      truncate
      floor
      ceiling
      round
      abs
      mod
      gcd
      random
      +
      -
      *
      /
      %
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
      binary*
      binary/
      binary%
      binary-mod
      binary-gcd

      ;; Numeric comparison functions
      <
      <=
      =
      >=
      >

      ;; String functions
      string-size
      string-null?
      string-append
      string-ref
      substring

      ;; I/O functions
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
      sflush
      flush

      char-ready?
      peek-char

      ;; Print control functions
      print-breadth
      print-depth

      ;; File i/o functions
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

      ;; Telos
      call-next-method
      next-method?
      allocate
      describe
      class?
      subclass?

      ;; Tables
      table-ref
      table-comparator
      table-delete
      table-size
      table-keys
      table-values
      table-fill
      table-clear

      ;; Plus some others
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

      time-start
      time-stop
      time

      backtrace
      backtrace?

      ;; Thread
      <thread>
      <simple-thread>
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

      ;; Telos classes
      <object>
      <class>
      <simple-class>
      <list>
      <cons>
      <null>
      <number>
      <integer>
      <int>
      <float>
      <double-float>
      <symbol>
      <keyword>
      <string>
      <stream>
      <input-stream>
      <output-stream>
      <i/o-stream>
      <vector>
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

      ;; Setter
      setter

      ;; Converter
      converter

      convert
      <conversion-condition>
      <no-converter>

      ;; Condition
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

      ;; Comparison
      binary<
      binary=
      <
      =
      >
      <=
      >=
      max
      min

      ;; Macros
      defmacro
      quasiquote
      unquote
      unquote-splicing
      macroexpand
      macroexpand1
      ;   syntax
      ;   dprint

      ;; Collect
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

      ;; Copy
      deep-copy
      shallow-copy

      ;; Format
      sformat
      format
      fmt

      ;; Module functions
      defmodule
      import
      syntax
      export
      expose

      module-symbols
      module-exports
      symbol-module
      current-module
      module-list
      unintern

      enter-module
      !>
      reenter-module
      !>>

      ; EuXLisp utility functions
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

      ;; EuXLisp debugging functions
      trace-on
      trace-off
      ))

  (defmacro time (expr stream)
    (let ((x (gensym "time"))
          (res (gensym "time")))
      `(let* ((,x (time-start))
             (,res ,expr))
         (time-stop ,x)
         (sformat ,stream
                  "real: ~a\nuser: ~a\nsystem: ~a\n"
                  (vector-ref ,x 0) (vector-ref ,x 1) (vector-ref ,x 2))
         ,res)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
