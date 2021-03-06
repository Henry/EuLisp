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
;;; Title: Symbols not usually exported from telos level-0
;;;  Description:
;;    Mainly introspective functionality.
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule telosint
  (import (root)
   export (next-methods
           arg-list
           class-of
           class-name
           class-superclasses
           class-precedence-list
           class-slots
           class-keywords
           set-class-keywords
           class-subclasses
           class-instance-size
           class-abstract?
           class?
           subclass?
           generic-name
           generic-args
           set-generic-args
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
           set-slot-default
           slot-required?
           set-slot-required?
           find-slot-index
           initialize-object
           initialize-class
           builtin-make-table
           print-class-name))

; install <class> slot defaults
(deflocal slots (class-slots <class>))

(set-slot-default (list-ref slots 0)
                  (lambda () 'anon)) ; name
(set-slot-default (list-ref slots 1)
                  (lambda () (list <object>))) ; superclasses
(set-slot-default (list-ref slots 5)
                  (lambda () ()))    ; subclasses
(set-slot-default (list-ref slots 7)
                  (lambda () ()))    ; abstract?

; install <simple-class> slot defaults
(deflocal sslots (class-slots <simple-class>))

(set-slot-default (list-ref sslots 0)
                  (lambda () 'anon)) ; name
(set-slot-default (list-ref sslots 1)
                  (lambda () (list <object>))) ; superclasses
(set-slot-default (list-ref sslots 5)
                  (lambda () ()))    ; subclasses
(set-slot-default (list-ref sslots 7)
                  (lambda () ()))    ; abstract?

;; tweaks to allow make of a few built-in classes
(deflocal builtin-make-table (make-table eq))

;; cons
(defun mkcons (inits)
  (let ((car (find-key car: inits ()))
        (cdr (find-key cdr: inits ())))
    (cons car cdr)))

(set-class-keywords <cons> '(car: cdr:))
(table-set builtin-make-table <cons> mkcons)

;; null
(defun mknull (inits)
  ())

(table-set builtin-make-table <null> mknull)

;; int
(defun mkint (inits)
  (find-key value: inits 0))

(set-class-keywords <fpi> '(value:))
(table-set builtin-make-table <fpi> mkint)

;; double-float
(defun mkdfloat (inits)
  (find-key value: inits 0.0))

(set-class-keywords <double-float> '(value:))
(table-set builtin-make-table <double-float> mkdfloat)

;; symbol
(defun mksymbol (inits)
  (let ((name (find-key string: inits "")))
    (string->symbol name)))

(set-class-keywords <symbol> '(string:))
(table-set builtin-make-table <symbol> mksymbol)

;; keyword
(defun mkkeyword (inits)
  (let* ((name (find-key string: inits ""))
         (len (string-size name)))
    (if (%= len 0)
        :                             ; null keyword
      (string->symbol
       (if (eql (string-ref name (%- len 1)) #\:)
           name
         (string-append name ":"))))))

(set-class-keywords <keyword> '(string:))
(table-set builtin-make-table <keyword> mkkeyword)

;; string
(defun mkstring (inits)
  (let ((size (find-key size: inits 0))
        (fill (find-key fill-value: inits #\space)))
    (make-string
     size
     (if (integer? fill)
         (integer->char fill)
       fill))))

(set-class-keywords <string> '(size: fill-value:))
;;***HGW (set-class-keywords <string> (class-keywords <string>))
(table-set builtin-make-table <string> mkstring)

;; input-stream
(defun mkistream (inits)
  (let ((filename (find-key filename: inits ())))
    (open-input-file filename)))

(set-class-keywords <input-stream> '(filename:))
(table-set builtin-make-table <input-stream> mkistream)

;; output-stream
(defun mkostream (inits)
  (let ((filename (find-key filename: inits ())))
    (open-output-file filename)))

(set-class-keywords <output-stream> '(filename:))
(table-set builtin-make-table <output-stream> mkostream)

;; i/o stream ?

;; vector
(defun mkvector (inits)
  (let ((size (find-key size: inits 0))
        (fill (find-key fill-value: inits ())))
    (make-vector size fill)))

(set-class-keywords <vector> '(size: fill-value:))
;;***HGW (set-class-keywords <vector> (class-keywords <vector>))
(table-set builtin-make-table <vector> mkvector)

;; char
(defun mkchar (inits)
  (let ((code (find-key code: inits 32)))
    (integer->char code)))

(set-class-keywords <char> '(code:))
(set-class-keywords <simple-char> (class-keywords <char>))
(table-set builtin-make-table <simple-char> mkchar)

;; promise, env, code, module, simple-function, closure,
;; fun, xfun, xfuncont, continuation, slot
(defun cant-make (cl)
  (lambda (inits)
    (raise-telos-error "can't make instance of class" cl)))

(table-set builtin-make-table <promise> (cant-make <promise>))
(table-set builtin-make-table <env> (cant-make <env>))
(table-set builtin-make-table <code> (cant-make <code>))
(table-set builtin-make-table <module> (cant-make <module>))
(table-set builtin-make-table <simple-function>
           (cant-make <simple-function>))
(table-set builtin-make-table <closure> (cant-make <closure>))
(table-set builtin-make-table <fun> (cant-make <fun>))
(table-set builtin-make-table <xfun> (cant-make <xfun>))
(table-set builtin-make-table <xfuncont> (cant-make <xfuncont>))
(table-set builtin-make-table <continuation> (cant-make <continuation>))

;; slot
(set-class-keywords <slot> '(keyword: default: required?:))
(set-class-keywords <local-slot> (class-keywords <slot>))
(table-set builtin-make-table <local-slot> (cant-make <slot>))

;; table
(defun mktable (inits)
  (let ((comp (find-key comparator: inits eql))
        (fill (find-key fill-value: inits ())))
    (make-table comp fill)))

(set-class-keywords <table> '(comparator: fill-value:))
(set-class-keywords <hash-table> (class-keywords <table>))
(table-set builtin-make-table <hash-table> mktable)

;; for thread see thread.em

;; generic
(defun mkgeneric (inits)
  (let ((name (find-key name: inits 'anonymous-generic))
        (domain (find-key domain: inits ()))
        (optargs? (find-key optargs: inits ())))
    (if (null? domain)
        (raise-telos-error "missing keyword domain: in make <simple-generic>"
                           inits))
    (make-generic name domain optargs?)))

(set-class-keywords <generic> '(name: domain: optargs:))
(set-class-keywords <simple-generic> (class-keywords <generic>))
(table-set builtin-make-table <simple-generic> mkgeneric)

;; method
(defun mkmethod (inits)
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

(set-class-keywords <method> '(method-lambda: domain: optargs:))
(set-class-keywords <simple-method> (class-keywords <method>))
(table-set builtin-make-table <simple-method> mkmethod)

(defun strip<> (name)
  (if (not (symbol? name))
      (raise-telos-error "class name not a symbol" name))
  (let* ((str (symbol->string name))
         (len (string-size str)))
    (if (and (%> len 1)
             (eql (string-ref str 0) #\<)
             (eql (string-ref str (%- len 1)) #\>))
        (substring str 1 (%- len 1))
      str)))

(defun print-class-name (obj stream)
  (%print (strip<> (class-name (class-of obj))) stream))

;;;-----------------------------------------------------------------------------
)  ;; End of module telosint
;;;-----------------------------------------------------------------------------
