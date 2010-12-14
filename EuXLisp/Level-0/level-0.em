;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp Level-0 module
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule level-0
  (syntax (syntax-0)
   import (;; all of the level-0 modules
           root
           system
           thread
           telos
           telosint
           setter
           convert
           condition
           number
           compare
           collection
           copy
           formatted-io
           math
           eval)
   export (;; Special operators
           deflocal
           defconstant
           defclass
           defun
           defgeneric
           defmethod
           quote
           lambda
           .l
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
           letfuns
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
           list-size
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

           sprint
           print
           sflush
           flush
           nl

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
           else
           system
           getenv
           putenv
           tmpfile

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
           <fpi>
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

           generic-print
           generic-write
           wait

           make
           initialize
           class-hierarchy

           ;; Setter
           setter
           fsetter

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
           <compilation-general-error>
           <macro-error>
           <socket-error>
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

           ; EuXLisp specific functions
           enter-module
           !>
           reenter-module
           !>>
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
           backtrace
           backtrace?

           ;; EuXLisp/Youtoo compatible Extensions
           ticks-per-second
           cpu-time
           time-execution)
   expose (eval))

(defmacro time-execution (expr stream)
  (let ((x (gensym "time"))
        (res (gensym "time")))
    `(let* ((,x (cpu-time))
            (,res ,expr))
       (setq ,x (map (lambda (x y)
                       (/ (binary- x y)
                          (convert ticks-per-second <double-float>)))
                     (cpu-time) ,x))
       (sprint ,stream
               "real: "     (vector-ref ,x 0)
               "\nuser: "   (vector-ref ,x 1)
               "\nsystem: " (vector-ref ,x 2)
               nl)
       ,res)))

;;;-----------------------------------------------------------------------------
)  ;; End of module level-0
;;;-----------------------------------------------------------------------------
