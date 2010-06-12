;;; system.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford
;;
;;; extras to access system internals
;;; those symbols not exported from level0

(defmodule system
    (import (root)
     export
     (%MAP1
      %FOR-EACH1
      %WITH-FILE1
      %LOAD1
      %FORCE1
      %INITLOOP1
      %CAR
      %CDR
      %SET-CAR!
      %SET-CDR!
      %VECTOR-LENGTH
      %VECTOR-REF
      %VECTOR-SET!
      %KEYWORDS
      %MAKE-CONSTANT
      %IMPORT

      *INTIALIZE*
      *TOPLEVEL*
      **EOF**
      **DEFAULT**
      *UNBOUND*
      stdin
      stdout
      stderr
      *FIXNUM-FORMAT*
      *FLONUM-FORMAT*
      *PRINT-CASE*
      *gc-msgs*

      ; setters
      set-car!
      set-cdr!
      vector-set!
      array-set!
      set-file-position!
      set-symbol-value!
      set-symbol-plist!
      table-set!
      set-table-fill!
      string-set!

      set-module
      find-module
      ;   reintern
      reintern-syntax
      ;   %syntax
      ;   %macro
      ;   %rename
      get-syntax
      put-syntax
      qualified-symbols?

      define-generic
      define-method

      getbcode
      getliteral
      setivar
      getivar
      tmpfile

      the-environment
      procedure-environment
      environment?
      environment-bindings
      environment-parent

      check-ref
      ))

  (define (getbcode closure)
          (%VECTOR-REF (%CAR closure) 0))

  (define (getliteral closure n)
          (%VECTOR-REF (%CAR closure) n))
  )
