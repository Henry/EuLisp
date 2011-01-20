;;; eulisp-mode-el -- Major mode for editing EuLisp files
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;; Maintainer: Henry G. Weller
;;
;; Created: Wed Aug 19 23:24:17 2009 (+0100)
;; Version: 0.6
;; Last-Updated: Tue Aug 24 16:31:00 2010 (+0100)
;;           By: Henry G. Weller
;;     Update #: 8
;; URL: Not yet available
;; Keywords: EuLisp major-mode
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; The major mode for editing EuLisp code.
;;
;; Based heavily on lisp-mode and scheme-mode with inspiration from font-lock,
;; python-mode and simula-mode supplied with emacs-23 and a couple of ideas from
;; clojure-mode.
;;
;; Two commenting styles are supported:
;;
;;     ; single-line comments
;;     #; (statement comments)
;;
;; Note than the eulisp-font-lock-keywords were obtained from the Youtoo
;; Bindings.txt file not from the EuLisp definition and hence contain many
;; keywords not in the definition.  This will be rectified in due course.
;; -----------------------------------------------------------------------------
;;; Change log:
;;
;; Version 0.1
;; * Initial release.
;; Version 0.2
;; * Added support for s-expression comments.
;; Version 0.3
;; * Added special indentation handling for EuLisp infix symbols defined in
;;   `eulisp-special-symbol-indent-regexp' e.g. `->' which if on new line are
;;   indented one level."
;; Version 0.4
;; * Added special indentation handling for forms for which all but the last
;;   specified number of arguments should be indented.  This is useful for the
;;   EuLisp `let' in which the last argument is special.
;; * Added indentation of the arguments in a function call form.
;; * Split `define' and 'datatype' font-locking so that the function and type
;;   names can have different fonts.
;; * Changed the default indentation style of `if' to indent the condition and
;;   execution statements the same amount.
;; Version 0.5
;; * Updated predicates.
;; Version 0.6
;; * Improved indentation rule:
;;     + default indentation for defmodule now 0;
;;     + defmodule directives list now indented 1 level;
;;     + `defun' indentation added for Eu2C-specific defining forms.
;; -----------------------------------------------------------------------------
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; -----------------------------------------------------------------------------
;;; Code:

;; Include support for better commenting/un-commenting
(require 'newcomment)

;; -----------------------------------------------------------------------------
;;;  Customization variables

(defgroup eulisp nil
  "Editing mode for the EuLisp programming language."
  :group 'languages
  :version "0.3"
  :link '(emacs-commentary-link "eulisp"))

;;;###autoload
(defcustom eulisp-special-symbol-indent-regexp "->"
  "Regular expression for the EuLisp symbols to be indented one additional level."
  :group 'eulisp
  :type 'regexp)

;;;###autoload
(defcustom eulisp-indent-optional-function-alist nil
  "Alist of indentation methods for standard EuLisp functions.
Each element is a cons-cell (FUNCTION . INDENTATION-METHOD)."
  :group 'eulisp
  :type
  '(alist
    :key-type symbol
    :value-type
    (choice

     (const :tag "Handle this function like a `def' construct: treat the
second line as the start of a `body'" defun)

     (integer :tag "The first NUMBER arguments of the
function are `distinguished' arguments; the rest are considered
the body of the expression.  A line in the expression is indented
according to whether the first argument on it is distinguished or
not.  If the argument is part of the body, the line is indented
`lisp-body-indent' more columns than the open-parenthesis
starting the containing expression.  If the argument is
distinguished and is either the first or second argument, it is
indented _twice_ that many extra columns.  If the argument is
distinguished and not the first or second argument, the line uses
the standard pattern.")

     (symbol :tag "SYMBOL should be a function name; that
function is called to calculate the indentation of a line within
this expression.  The function receives two arguments:

STATE
    The value returned by `parse-partial-sexp' (a Lisp
    primitive for indentation and nesting computation) when it
    parses up to the beginning of this line.

POS
    The position at which the line being indented begins.

It should return either a number, which is the number of columns
of indentation for that line, or a list whose car is such a
number.  The difference between returning a number and returning
a list is that a number says that all following lines at the same
nesting level should be indented just like this one; a list says
that following lines might call for different indentations.  This
makes a difference when the indentation is being computed by
`C-M-q'; if the value is a number, `C-M-q' need not recalculate
indentation for the following lines until the end of the list."))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.em\\'" . eulisp-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.es\\'" . eulisp-mode))

;;;###autoload
(add-to-list 'same-window-buffer-names "*EuLisp*")

;; -----------------------------------------------------------------------------
;;;  EuLisp mode font-locking

(defconst eulisp-font-lock-keywords-1
  (eval-when-compile
    (list
     '("(\\(defmodule\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-function-name-face nil t))
     '("(\\(defclass\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-function-name-face nil t))
     '("(\\(defgeneric\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-function-name-face nil t))
     '("(\\(defmethod\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-function-name-face nil t))
     '("(\\(defconstant\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-type-face nil t))
     '("(\\(deflocal\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-type-face nil t))
     '("(\\(defglobal\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-type-face nil t))
     '("(\\(defmacro\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-function-name-face nil t))
     '("(\\(defun\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-function-name-face nil t))
     '("(\\(defcondition\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-function-name-face nil t))
     '("(\\(make\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-builtin-face) (2 font-lock-type-face nil t))
     '("\\<\\(error\\)\\>\\(.*\\)"
       (1 font-lock-warning-face) (2 font-lock-warning-face nil t))
     '("\\<\\(cerror\\)\\>\\(.*\\)"
       (1 font-lock-warning-face) (2 font-lock-warning-face nil t))
     ))
  "Highlight function declarations, strings, comments and errors.")

(defconst eulisp-font-lock-keywords-2
  (append eulisp-font-lock-keywords-1
   (eval-when-compile
     (list
      ;; Control structures
      (cons
       (concat
        "(" (regexp-opt
             '("if" "let/cc" "letfuns" "labels" "progn" "unwind-protect" "apply"
               "cond" "and" "or" "block" "return-from"  "let" "let*"
               "while" "match") t) "\\>")
       '(1 font-lock-builtin-face))
      ;; Control modifiers
      (cons
       (regexp-opt
        '("where") 'words)
       'font-lock-builtin-face)
      ;; Built-in types
      (cons
       (regexp-opt
        '("<object>"
          "<symbol>" "<number>"
          "<integer>" "<fixed-precision-integer>" "<float>" "<double-float>"
          "<string>"
          "<collection>" "<sequence>" "<cons>" "<vector>"
          "<table>" "<hash-table>" "<null>"
          "<condition>" "<special>"
          "<slot-description>" "<local-slot-description>"
          "<stream>" "<file-stream>" "<char-file-stream>" "<string-stream>"
          "<thread>" "<lock>") 'words)
       'font-lock-type-face)
      ;; Built-in constants
      (cons
       (regexp-opt
        '("t" "nil") 'words)
       'font-lock-constant-face)
      ;; Tuples
      '("[\(\[]\\(@p\\)\\>" (1 font-lock-type-face))
    )))
  "In addition to level 1, highlight all language keywords, including type names
and named constant values.")

(defconst eulisp-font-lock-keywords-3
  (append eulisp-font-lock-keywords-2
   (eval-when-compile
     (list
      ;; Directives
      (cons
       (regexp-opt
        '("import" "syntax" "expose" "export")
        'words) 'font-lock-keyword-face)
      ;; System functions
      (cons
       (concat
        "("
        (regexp-opt
         '(;; Bindings in Lib.x86_64/liblevel-1
           "%"
           "*"
           "+"
           "-"
           "/"
           "<"
           "<="
           "<abstract-thread>"
           "<buffered-stream>"
           "<character-sequence>"
           "<character>"
           "<class>"
           "<collection>"
           "<condition>"
           "<connection>"
           "<cons>"
           "<double*>"
           "<double-float>"
           "<double-float>"
           "<end-of-stream>"
           "<file-control-block>"
           "<file-stream>"
           "<fixed-precision-integer>"
           "<float>"
           "<fpi>"
           "<function-class>"
           "<function>"
           "<generic-function>"
           "<handler>"
           "<hash-table>"
           "<int*>"
           "<fpi>"
           "<integer>"
           "<keyword>"
           "<list>"
           "<local-slot>"
           "<lock>"
           "<method>"
           "<name>"
           "<null>"
           "<number>"
           "<object>"
           "<semaphore>"
           "<sequence>"
           "<simple-class>"
           "<simple-function>"
           "<simple-generic-function>"
           "<simple-hash-table>"
           "<simple-method>"
           "<simple-thread>"
           "<slot>"
           "<socket>"
           "<state>"
           "<stream-condition>"
           "<stream-control-block>"
           "<stream>"
           "<string*>"
           "<string-stream>"
           "<string>"
           "<symbol>"
           "<system-condition>"
           "<table>"
           "<thread>"
           "<vector>"
           "="
           ">"
           ">="
           "abs"
           "accumulate"
           "accumulate-list"
           "accumulate-vector"
           "accumulate1"
           "accumulate1-list"
           "accumulate1-vector"
           "add-method"
           "add-subclass"
           "allocate"
           "all?"
           "allp1-table"
           "allp1-vector"
           "alnum?"
           "alpha?"
           "any?"
           "anyp1-list"
           "anyp1-table"
           "anyp1-vector"
           "anyp2-list"
           "append"
           "append!"
           "apply"
           "as-lowercase"
           "as-proper-list"
           "as-uppercase"
           "assoc-list-ref"
           "atom?"
           "backtrace"
           "binary%"
           "binary*"
           "binary+"
           "binary-"
           "binary-gcd"
           "binary-lcm"
           "binary-mod"
           "binary/"
           "binary<"
           "binary="
           "bit-and"
           "bit-ior"
           "bit-not"
           "bit-shift"
           "bit-xor"
           "buffered-stream?"
           "caaar"
           "caadr"
           "caar"
           "cadar"
           "cadddr"
           "caddr"
           "cadr"
           "call/ep"
           "call1/cc"
           "car"
           "cdaar"
           "cdadr"
           "cdar"
           "cddar"
           "cdddr"
           "cddr"
           "cdr"
           "ceiling"
           "cerror"
           "character-as-string"
           "character?"
           "class-abstract?"
           "class-code"
           "class-direct-subclasses"
           "class-direct-superclasses"
           "class-instance-length"
           "class-keywords"
           "class-keywordz"
           "class-name"
           "class-of"
           "class-precedence-list"
           "class-slot-defaults"
           "class-slots"
           "class-slotz"
           "class?"
           "clear-table"
           "collection?"
           "compatible-superclass?"
           "compatible-superclasses?"
           "compute-and-ensure-slot-accessors"
           "compute-class-codes"
           "compute-class-precedence-list"
           "compute-defined-slot"
           "compute-defined-slot-class"
           "compute-discriminating-function"
           "compute-inherited-keywords"
           "compute-inherited-slots"
           "compute-keywords"
           "compute-method-lookup-function"
           "compute-primitive-reader-using-class"
           "compute-primitive-reader-using-slot"
           "compute-primitive-writer-using-class"
           "compute-primitive-writer-using-slot"
           "compute-slot-reader"
           "compute-slot-writer"
           "compute-slots"
           "compute-specialized-slot"
           "compute-specialized-slot-class"
           "concatenate"
           "condition-message"
           "condition?"
           "connect"
           "connection-host"
           "connection-port"
           "connection?"
           "cons"
           "cons-keywords"
           "cons-slot-defaults"
           "cons-slots"
           "cons?"
           "control-block-buffer"
           "control-block-buffer-cnt"
           "control-block-buffer-pos"
           "control-block-buffer-size"
           "control-block-descriptor"
           "control-block-file-name"
           "control-block-mode"
           "convert"
           "converter"
           "cpl-subclass?"
           "current-thread"
           "current-thread-queue"
           "dec"
           "deep-copy"
           "delete"
           "digit?"
           "disconnect"
           "discriminating-domain"
           "do"
           "do1-list"
           "do1-list-last-special"
           "do1-string"
           "do1-table"
           "do1-vector"
           "do2-list"
           "dot"
           "double-float?"
           "dynamic-variable-ref"
           "element"
           "empty?"
           "end-of-stream"
           "ensure-slot-reader"
           "ensure-slot-writer"
           "eos"
           "eos-default-value"
           "eq"
           "eql"
           "equal"
           "error"
           "error-no-applicable-methods"
           "even?"
           "exit"
           "file-control-block?"
           "file-lookup"
           "file-stream?"
           "fill"
           "fill-buffer"
           "filter-keywords"
           "finalize"
           "find"
           "find-key"
           "find-slot"
           "find-slot-names"
           "find1-list"
           "fixed-precision-integer?"
           "float?"
           "floor"
           "flush"
           "flush-buffer"
           "format"
           "format1"
           "fprintf"
           "from-stream"
           "function-domain"
           "function-name"
           "function-slot-defaults"
           "function?"
           "gcd"
           "generic-connect"
           "generic-function-discriminating-function"
           "generic-function-domain"
           "generic-function-method-cache"
           "generic-function-method-class"
           "generic-function-method-keywords"
           "generic-function-method-lookup-function"
           "generic-function-methods"
           "generic-function?"
           "generic-print"
           "generic-read"
           "generic-write"
           "gensym"
           "getenv"
           "gf-keywords"
           "gf-reset-cache"
           "gf-slot-defaults"
           "gf-slots"
           "graph?"
           "handle"
           "handler?"
           "hash-table?"
           "hostname"
           "inc"
           "init-class"
           "init-list-ref"
           "initialize"
           "install-callback"
           "int-binary%"
           "int-binary*"
           "int-binary+"
           "int-binary-"
           "int-binary-mod"
           "int-binary/"
           "int-binary<"
           "int-binary="
           "int-zero?"
           "integer?"
           "fpi?"
           "keyword-name"
           "keyword?"
           "lcm"
           "lispin"
           "list"
           "list-drop"
           "list-equal"
           "list-ref"
           "list-remove"
           "list-remove-duplicates"
           "list-size"
           "list-start"
           "list-stop"
           "listify-env-string"
           "listify-string"
           "list?"
           "lock"
           "lock?"
           "lowercase?"
           "lsd-keywords"
           "lsd-slot-defaults"
           "lsd-slots"
           "make"
           "make-generic-function"
           "make-method"
           "make-space"
           "make-vector"
           "map"
           "map1-list"
           "map1-list-last-special"
           "map1-string"
           "map1-table"
           "map1-vector"
           "map2-list"
           "mapcan"
           "max"
           "member"
           "member-list"
           "member1-list"
           "member1-vector"
           "method-domain"
           "method-function"
           "method-generic-function"
           "method-keywords"
           "method-slot-defaults"
           "method-slots"
           "method?"
           "min"
           "mod"
           "mode-table"
           "name"
           "name-keywords"
           "name-slot-defaults"
           "name-slots"
           "negate"
           "negative?"
           "newline"
           "null?"
           "number?"
           "object-keywords"
           "object-slots"
           "object?"
           "odd?"
           "open-file-streams"
           "output-condition-contents"
           "output-list-contents"
           "permute"
           "pop-dynamic-variables"
           "pop-error-handlers"
           "positive?"
           "pprint"
           "predefined-reader"
           "predefined-writer"
           "primitive-allocate"
           "primitive-class-of"
           "primitive-find-slot-position"
           "primitive-metaclass?"
           "primitive-prin"
           "primitive-print"
           "primitive-ref"
           "primitive-slot-value"
           "prin"
           "prin-address"
           "prin-char"
           "prin-one-char"
           "prin-string"
           "print"
           "proper-list?"
           "push-dynamic-variable"
           "push-error-handler"
           "quasiquote-mark"
           "quote-mark"
           "random"
           "random-seed"
           "random-true-nil"
           "read"
           "read-char"
           "read-line"
           "read-s-expression"
           "read-token"
           "reconnect"
           "remove"
           "remove-class"
           "reset"
           "reverse"
           "reverse!"
           "reverse-list"
           "reverse-list!"
           "reverse-vector"
           "reverse-vector!"
           "round"
           "sd-keywords"
           "sd-slots"
           "select"
           "select-list"
           "semaphore-counter"
           "semaphore?"
           "sequence?"
           "set-dispatch-macro-character"
           "setter"
           "sf-direct-slot-defaults"
           "sf-direct-slots"
           "sf-slot-defaults"
           "sf-slots"
           "shallow-copy"
           "sig="
           "signal"
           "signum"
           "simple-function-code"
           "simple-function-environment"
           "simple-function?"
           "simple-generic-function?"
           "simple-hash-table?"
           "simple-thread?"
           "size"
           "slot-default"
           "slot-keyword"
           "slot-name"
           "slot-reader"
           "slot-required?"
           "slot-value"
           "slot-value-using-slot"
           "slot-writer"
           "slot?"
           "socket-descriptor"
           "socket-host"
           "socket-port"
           "socket-queue-size"
           "sort"
           "sort!"
           "sort-list"
           "special-tokens"
           "sprin"
           "sprintf"
           "sread"
           "sread-s-expression"
           "stable-add-method"
           "stack-values"
           "state-context-stack"
           "state-context-stack-size"
           "state-value-stack"
           "state-value-stack-size"
           "stderr"
           "stdin"
           "stdout"
           "stream-control-block?"
           "stream-lock"
           "stream-mode"
           "stream-read-action"
           "stream-sink"
           "stream-source"
           "stream-write-action"
           "stream?"
           "strerror"
           "string-append"
           "string-data"
           "string-empty?"
           "string-equal"
           "string-ref"
           "string-size"
           "string-stream?"
           "string-stream-string-list"
           "string?"
           "subclass?"
           "substring"
           "subvector"
           "symbol-exists?"
           "symbol-name"
           "symbol?"
           "system"
           "table-comparator"
           "table-empty?"
           "table-entries"
           "table-fill-value"
           "table-hash-function"
           "table-keys"
           "table-ref"
           "table-size"
           "table-threshold"
           "table-values"
           "table?"
           "tconc"
           "the-method-lookup-function"
           "thread-block"
           "thread-continuation"
           "thread-dynamic-variables"
           "thread-error-handlers"
           "thread-reschedule"
           "thread-return-value"
           "thread-returned?"
           "thread-start"
           "thread-state"
           "thread-suspend"
           "thread-unblock"
           "thread-value"
           "thread?"
           "ticks-per-second"
           "time-start"
           "time-stop"
           "to-stream"
           "truncate"
           "unlock"
           "unquote-mark"
           "unquote-splicing-mark"
           "uppercase?"
           "vector-append"
           "vector-empty?"
           "vector-ref"
           "vector-size"
           "vector-start"
           "vector-stop"
           "vector?"
           "wait"
           "warning"
           "write"
           "zero?"

           ;; Bindings in Runtime/macros
           "and"
           "block"
           "butlast"
           "catch"
           "cond"
           "defclass"
           "defcondition"
           "defgeneric"
           "defmethod"
           "defmethod-args"
           "defmethod-body"
           "defmethod-domain"
           "defmethod-keywords"
           "defmethod-sig"
           "defprimclass"
           "defvar"
           "dynamic"
           "dynamic-let"
           "dynamic-setq"
           "for"
           "generic-lambda"
           "get-global-register"
           "last"
           "let/cc"
           "match-let"
           "method-function-lambda"
           "method-lambda"
           "named-method-function-lambda"
           "not"
           "or"
           "return-from"
           "set-global-register"
           "throw"
           "time"
           "unless"
           "unwind-protect"
           "when"
           "while"
           "with-handler"
           "with-input-file"
           "with-input-file-of-path"
           "with-lock"
           "with-output-file"
           "with-sink"
           "with-source"

           ;; Bindings in Lib.x86_64/libeval
           "eval"
           ) t) "\\>") 'font-lock-keyword-face)
      ;; User-defined types with angle-brackets
      '("\\<<\\w+>\\>" . font-lock-type-face)
      ;; Global variables with *earmuffs*
      '("\\<\\*\\w+\\*\\>" . font-lock-constant-face)
      ;; Variable names starting with a capital letter
      '("\\<[A-Z]\\w*\\>" . font-lock-variable-name-face)
      ;; Function name in a call e.g. (func-name args)
      '("(\\(\\w*\\)\\>" (1 font-lock-function-name-face))
      )))
   "In addition to level 2, highlight the symbols being defined in functions and
variable declarations, and all builtin function names, wherever they appear.")

(defconst eulisp-font-lock-headings
  (eval-when-compile
    (list
     '("^;;; [^ ].*" 0 'outline-2 t)
     '("^;;;  [^ ].*" 0 'outline-3 t)
     '("^;;;   [^ ].*" 0 'outline-4 t)
     ))
  "Set the fonts for three levels of headings in EuLisp mode.")

(defvar eulisp-font-lock-keywords
  (append eulisp-font-lock-keywords-3 eulisp-font-lock-headings)
  "Default expressions to highlight in EuLisp mode.")

(defun eulisp-font-lock ()
  "Set up font-locking"
  (interactive)
  ;;(set (make-local-variable 'font-lock-defaults) '(eulisp-font-lock-keywords))
  (set (make-local-variable 'font-lock-defaults)
       '((eulisp-font-lock-keywords)
         nil nil (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 124"))
         beginning-of-defun
         (font-lock-mark-block-function . mark-defun)
         (font-lock-syntactic-face-function
          . eulisp-font-lock-syntactic-face-function)
         (parse-sexp-lookup-properties . t)
         (font-lock-extra-managed-props syntax-table)))
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *"))

;; -----------------------------------------------------------------------------
;;;  EuLisp mode syntax table

;; Setup the syntax table based heavily on that for Lisp in lisp-mode.el and
;; adapted for EuLisp:
;; [ and ] are open and close delimiter characters.
(defvar eulisp-mode-syntax-table
  (let ((st (make-syntax-table)))
    (let ((i 0))
      (while (< i ?0)
        (modify-syntax-entry i "_   " st)
        (setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
        (modify-syntax-entry i "_   " st)
        (setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
        (modify-syntax-entry i "_   " st)
        (setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
        (modify-syntax-entry i "_   " st)
        (setq i (1+ i)))
      ;; Used in flonum symbols
      (modify-syntax-entry ?. "_    " st)
      (modify-syntax-entry ?\s "    " st)
      ;; Non-break space acts as whitespace.
      (modify-syntax-entry ?\x8a0 " " st)
      (modify-syntax-entry ?\t "    " st)
      (modify-syntax-entry ?\f "    " st)
      (modify-syntax-entry ?` "'    " st)
      (modify-syntax-entry ?' "'    " st)
      (modify-syntax-entry ?, "'    " st)
      (modify-syntax-entry ?@ "'    " st)
      (modify-syntax-entry ?\" "\"  " st)
      (modify-syntax-entry ?\\ "\\  " st)
      (modify-syntax-entry ?\( "()  " st)
      (modify-syntax-entry ?\) ")(  " st)
      (modify-syntax-entry ?\[ "(]  " st)
      (modify-syntax-entry ?\] ")[  " st)
      (modify-syntax-entry ?\{ "(}  " st)
      (modify-syntax-entry ?\} "){  " st)

      ;; Define ; as the start of a line comment
      ;; And also ; as the second character in #;(...) sexp-comments.
      (modify-syntax-entry ?\; "< 2 " st)
      (modify-syntax-entry ?\n ">   " st)
      )
    st)
  "Syntax table for EuLisp-mode")

(defconst eulisp-statement-comment-syntax-table
  (let ((st (make-syntax-table eulisp-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st)
  "Additional syntax table for statement comments starting with `#;'.")

(defun eulisp-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
             (eq (char-after (nth 8 state)) ?#)
             (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's an statement-comment.  Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
            (end
             (condition-case err
                 (let ((parse-sexp-lookup-properties nil))
                   (goto-char (+ 2 (nth 8 state)))
                   ;; FIXME: this doesn't handle the case where the sexp
                   ;; itself contains a #; comment.
                   (forward-sexp 1)
                   (point))
               (scan-error (nth 2 err)))))
        (when (< pos (- end 2))
          (put-text-property pos (- end 2)
                             'syntax-table eulisp-statement-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

;; -----------------------------------------------------------------------------
;;;  EuLisp mode indentation

(defun calculate-eulisp-indent (&optional parse-start)
  "Return appropriate indentation for current line as EuLisp code.
In the usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression.

Based on `calculate-lisp-indent' but with better handling for the indentation
of the text in #| |# block-comments"
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state paren-depth
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (if parse-start
          (goto-char parse-start)
        (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (setq paren-depth (elt state 0)) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (= (point) calculate-lisp-indent-last-sexp)
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((integerp (elt state 4))
               ;; Inside a block comment
               ;; Indent start and end according to comment level
               (cond ((looking-at "^[ \t]*#|")
                      (+ (* (elt state 4) lisp-body-indent) normal-indent))
                     ((looking-at "^[ \t]*|#")
                      (+ (* (1- (elt state 4)) lisp-body-indent) normal-indent))
                     (t
                      ;; Otherwise indent following previous comment line
                      (skip-chars-backward " \t\n")
                      (while (and (not (bolp)))
                        (re-search-backward "\\(^[ \t]*#|\\)\\|^"))
                      ;; If the previous line is the beginning of the comment
                      ;; do not change the indentation
                      (if (match-end 1)
                          nil
                        ;; If the previous line is text indent to match
                        (skip-chars-forward " \t\n")
                        (prog1
                            (current-column)
                          (goto-char indent-point))))))
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (and (not (looking-back "^[ \t]*\\|([ \t]+"))
                                   (or (not containing-sexp)
                                       (< (1+ containing-sexp) (point))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp
                           (point)
                           calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (save-excursion (beginning-of-line) (point))
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))

(defun eulisp-indent-specform
  (count state indent-point normal-indent special-indent-factor)
  "Special-form indentation function based on `lisp-indent-specform'
but with the additional of handling negative COUNT which is treated as
the number of arguments at the end of the form to be excluded from special
indentation."
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lisp-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; In the case of a negative count add the total number of arguments
    ;; to obtain the number to be specially indented
    (if (< count 0)
        (save-excursion
          (while (and (not (eobp))
                      (condition-case ()
                          (progn
                            (setq count (1+ count))
                            (forward-sexp 1)
                            (not (looking-at "[ \t\n]*\\s)")))
                        (error nil))))
          (setq i count)))
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  If it one of first count forms use
        ;; `special-indent-factor' times `lisp-body-indent', else normal indent.
        ;; With `lisp-body-indent bound' to 2 (the default), this just happens
        ;; to work the same with if as the older code, but it makes
        ;; `unwind-protect', `condition-case', `with-output-to-temp-buffer',
        ;; et. al. much more tasteful.  The older, less hacked, behavior can be
        ;; obtained by replacing below with
        ;; `(list normal-indent containing-form-start)'.
        (if (<= (- i count) 1)
            (list (+ containing-form-column
                     (* special-indent-factor lisp-body-indent))
                  containing-form-start)
          (list normal-indent containing-form-start))
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
        normal-indent))))

(defun eulisp-indent-line (&optional whole-exp)
  "Indent current line as EuLisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one.

Special handling is included for EuLisp infix symbols in defined in
`eulisp-special-symbol-indent-regexp' e.g. `->' which if on new
line are indented one level.

Adapted from `lisp-indent-line'."
  (interactive "P")
  (let ((indent (calculate-eulisp-indent))
        shift-amt
        end
        (pos (- (point-max) (point)))
        (beg (progn (beginning-of-line) (point))))
    (skip-chars-forward " \t")
    (if (or (null indent) (looking-at ";;;"))
        ;; Don't alter indentation of a ;;; comment line
        ;; or a line that starts in a string.
        (goto-char (- (point-max) pos))
      (when (listp indent)
        (setq indent (car indent)))
      ;; Indent special EuLisp keywords if lisp-indent-offset is an integer
      (when (and (integerp lisp-indent-offset)
                 (looking-at eulisp-special-symbol-indent-regexp))
        ;;(when (looking-at eulisp-special-symbol-indent-regexp)
        (setq indent (+ indent lisp-body-indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent)))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))
    ;; If desired, shift remaining lines of expression the same amount.
    (and whole-exp (not (zerop shift-amt))
         (save-excursion
           (goto-char beg)
           (forward-sexp 1)
           (setq end (point))
           (goto-char beg)
           (forward-line 1)
           (setq beg (point))
           (> end beg))
         (indent-code-rigidly beg end shift-amt))))

(defun eulisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
It is used when indenting a line within a function call, to see if the
called function says anything special about how to indent the line.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a EuLisp function
which has a non-nil property `eulisp-indent-function',
that specifies how to do the indentation.  The property value can be
* `defun', meaning indent `defun'-style;
* `defmodule', meaning indent `defmodule'-style;
* a positive integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
* a negative integer N, meaning indent all but the last N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
* a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation.

Adapted from `lisp-indent-function'."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\w\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function
             (buffer-substring (point) (progn (forward-sexp 1) (point))))
            method)
        (setq method (get (intern-soft function) 'eulisp-indent-function))
        (cond ((eq method 'defun)          ; Definition
               (lisp-indent-defform state indent-point))
              ((eq method 'defmodule)      ; Module definition
               (eulisp-indent-specform
                2 state indent-point 0 1))
              ((integerp method)           ; Special form using standard indent
               (eulisp-indent-specform
                method state indent-point normal-indent 2))
              (method                      ; Special indentation function
               (funcall method state indent-point normal-indent)))))))

(defun lisp-indent-directive (state indent-point normal-indent)
  "Indentation function for module directives."
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
        (goto-char (car (cdr state)))
        (+ 1 (current-column)))))

(defun eulisp-put-indent (sym indent)
  "Helper function to specify the indentation style INDENT of the
given symbol SYM."
  (put sym 'eulisp-indent-function indent)
  (put (intern (format "eulisp/%s" (symbol-name sym)))
       'eulisp-indent-function indent))

(defvar eulisp-indent-function-alist
  '((defmodule . defmodule)
    (defclass . defun)
    (defgeneric . defun)
    (defmethod . defun)
    (defcondition . defun)
    (defconstant . defun)
    (deflocal . defun)
    (defmacro . defun)
    (defun . defun)
    (lambda . defun)
    (let/cc . defun)
    (import . lisp-indent-directive)
    (syntax . lisp-indent-directive)
    (expose . lisp-indent-directive)
    (export . lisp-indent-directive)
    (let . 1)
    (let* . 1)
    (letfuns . 1)
    (while . 1)
    (progn . 0)
    (if . 2)
    (cond . 0)
    (when . 1)
    (unless . 1)
    (unwind-protect . 1)
    (match . defun)
    (smatch . defun)
    (smatch0 . defun)
    (match-let . 1)

    (install-import-expander . defun)
    (install-directive-expander . defun)

    (defaccessors . defun)
    (def-descrs . defun)
    (define-basic-data-types . defun)
    (define-compiler-condition . defun)
    (defined-class . defun)
    (defined-fun-p . defun)
    (defined-generic-fun . defun)
    (defined-generic-fun-p . defun)
    (defined-named-const-p . defun)
    (defined-result-type . defun)
    (defined-static . defun)
    (defined-type . defun)
    (define-machine-data-types . defun)
    (define-special-sys-funs . defun)
    (define-tail . defun)
    (define-tail-sys-functions . defun)
    (define-trans . defun)
    (define-transformation . defun)
    (definition . defun)
    (definterface-arg-conversions . defun)
    (definterface-arg-names . defun)
    (definterface-arg-types . defun)
    (definterface-clc . defun)
    (definterface-lcl . defun)
    (def-list . defun)
    (def-literal-class . defun)
    (deflocal . defun)
    (def-lzs-object . defun)
    (defmacro-forms . defun)
    (def-mzs-object . defun)
    (defsetf . defun)
    (defstandardclass . defun)
    (def-strategic-lattice-type . defun)
    (defstruct . defun)
    (def-sys-lattice-type . defun)
    (deftrans . defun)
    (deftransdef . defun)
    (deftrans-literal-values . defun)
    (deftransmod . defun)
    (deftranssyn . defun)
    (def-write-remaining-strategic-lattice-types . defun)
    (def-write-super-strategic-lattice-type-p . defun)
    (def-write-super-strategic-lattice-types . defun)

    (declare . defun)
    (declare-c-function . defun)
    (declare-system-functions . defun)

    (%define-abstract-class . defun)
    (%define-constant . defun)
    (%defined-type . defun)
    (%define-function . defun)
    (%define-generic . defun)
    (%define-latticef-type . defun)
    (%define-lattice-type . defun)
    (%define-literal-expansion . defun)
    (%define-metaclass . defun)
    (%define-standard-class . defun)
    (%define-tail-class . defun)
    (%define-variable . defun)

    (%declare-external-class . defun)
    (%declare-external-constant . defun)
    (%declare-external-function . defun)
    (%declare-external-generic . defun)
    (%declare-external-symbol . defun)
    (%declare-external-variable . defun)

    (%annotate-function . defun)
    (%annotate-binding . defun)

    (make-instance . defun)
    )
  "Alist of indentation methods for standard EuLisp functions.")

(defun eulisp-put-indent-function-alist (function-alist)
  (mapcar (lambda (x)
            (eulisp-put-indent (car x) (cdr x))) function-alist))

(eulisp-put-indent-function-alist eulisp-indent-function-alist)

;; -----------------------------------------------------------------------------
;;;  Re-formatting functions
;; Reusing code from `slime-editing-commands'

(defvar eulisp-close-parens-limit nil
  "Maxmimum parens for `eulisp-close-all-sexp' to insert. NIL
means to insert as many parentheses as necessary to correctly
close the form.")

(defun eulisp-close-all-parens-in-sexp (&optional region)
  "Balance parentheses of open s-expressions at point.
Insert enough right parentheses to balance unmatched left parentheses.
Delete extra left parentheses.  Reformat trailing parentheses
Lisp-stylishly.

If REGION is true, operate on the region. Otherwise operate on
the top-level sexp before point."
  (interactive "P")
  (let ((sexp-level 0)
        point)
    (save-excursion
      (save-restriction
        (when region
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-max)))
        ;; skip over closing parens, but not into comment
        (skip-chars-backward ") \t\n")
        (when (eulisp-beginning-of-comment)
          (forward-line)
          (skip-chars-forward " \t"))
        (setq point (point))
        ;; count sexps until either '(' or comment is found at first column
        (while (and (not (looking-at "^[(;]"))
                    (ignore-errors (backward-up-list 1) t))
          (setq sexp-level (1+ sexp-level)))))
    (when (> sexp-level 0)
      ;; insert correct number of right parens
      (goto-char point)
      (dotimes (i sexp-level) (insert ")"))
      ;; delete extra right parens
      (setq point (point))
      (skip-chars-forward " \t\n)")
      (skip-chars-backward " \t\n")
      (let* ((deleted-region     (delete-and-extract-region point (point)))
             (deleted-text       (substring-no-properties deleted-region))
             (prior-parens-count (count ?\) deleted-text)))
        ;; Remember: we always insert as many parentheses as necessary
        ;; and only afterwards delete the superfluously-added parens.
        (when eulisp-close-parens-limit
          (let ((missing-parens (- sexp-level prior-parens-count
                                   eulisp-close-parens-limit)))
            (dotimes (i (max 0 missing-parens))
              (delete-char -1))))))))

(defun eulisp-beginning-of-comment ()
  "Move point to beginning of comment.
If point is inside a comment move to beginning of comment and return point.
Otherwise leave point unchanged and return NIL."
  (let ((boundary (point)))
    (beginning-of-line)
    (cond ((re-search-forward comment-start-skip boundary t)
           (point))
          (t (goto-char boundary)
             nil))))

(defun eulisp-reformat-defun ()
  "Reformat trailing parentheses Lisp-stylishly and reindent toplevel form."
  (interactive)
  (save-excursion
    (end-of-defun)
    (eulisp-close-all-parens-in-sexp)
    (beginning-of-defun)
    (indent-sexp)))

;; -----------------------------------------------------------------------------
;;; Better automatic commenting/un-commenting

;; From http://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the
end of the line, then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the
end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; -----------------------------------------------------------------------------
;;;  Key-bindings

(defvar eulisp-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\C-c\C-y" 'eulisp-shell)
    (define-key map "\C-c\C-c" 'comment-dwim-line)
    map)
  "EuLisp mode key map which inherits the standard Lisp key map.")

;; -----------------------------------------------------------------------------
;;;  Menu

(defvar eulisp-mode-basic-menu-items
  '(["Run EuLisp" eulisp-shell
     :help "Run EuLisp shell in separate buffer"
     :active (not (and (boundp inferior-eulisp-buffer)
                       (get-buffer-process inferior-eulisp-buffer)))]
    ["[Un]comment Region/Line" comment-dwim-line
     :help "Comment or uncomment each line in the region"
     :active mark-active]
    ["Indent Region" indent-region
     :help "Indent each nonblank line in the region"
     :active mark-active]
    ["Indent Line" indent-for-tab-command])
  "Basic EuLisp formatting menu.")

(easy-menu-define eulisp-menu eulisp-mode-map "EuLisp Mode menu"
  (append
   '("EuLisp" :help "EuLisp-specific Features")
   eulisp-mode-basic-menu-items))

;; -----------------------------------------------------------------------------
;;;  EuLisp mode

;;;###autoload
(define-derived-mode eulisp-mode fundamental-mode "EuLisp"
  "Major mode for editing EuLisp files.

Turns on Font Lock mode.

Supports Eldoc mode (only for functions, using a Python process),
Info-Look and Imenu.  In Outline minor mode, `define' lines count
as headers.

\\{eulisp-mode-map}"
  :group 'eulisp
  (set-syntax-table eulisp-mode-syntax-table)

  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-add) 1)

  ;; Look within the line for a ';' following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")

  (set (make-local-variable 'comment-column) 40)

  ;; Set the regexp used by outline-mode to find the headings
  (set (make-local-variable 'outline-regexp) ";;;[ ]+\\|(......\\|  (......")

  ;; Remove the number of ";;;" from the outline-level
  (set (make-local-variable 'outline-level) '(lambda () (- (outline-level) 3)))

  ;; Set the heading levels for promotion and demotion
  (setq outline-promotion-headings '(";;; " ";;;  " ";;;   "))

  ;; Set up font-locking
  (eulisp-font-lock)

  ;; Register EuLisp-specific indentation function
  ;; commented out for Stefan (set (make-local-variable 'lisp-indent-offset) 2)
  (set (make-local-variable 'indent-line-function) 'eulisp-indent-line)
  (set (make-local-variable 'lisp-indent-function) 'eulisp-indent-function)

  ;; The standard lisp comment indentation does not work well with EuLisp
  (set (make-local-variable 'comment-indent-function) 'comment-indent-default)

  ;; Process the optional alist of function indentation specifications
  (when eulisp-indent-optional-function-alist
      (eulisp-put-indent-function-alist eulisp-indent-optional-function-alist))
  )

(provide 'eulisp-mode)

;; -----------------------------------------------------------------------------
;;; eulisp-mode.el ends here
