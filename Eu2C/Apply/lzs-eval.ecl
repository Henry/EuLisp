;;; Copyright 1994-2010 Fraunhofer ISST
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'Eu2C'
;;;-----------------------------------------------------------------------------
;;
;;  Eu2C is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Eu2C is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Title: An interpreter working on LZS-representation of Lisp-programs
;;;  Description:
;;    The function EVAL evaluates an expression given in LZS-form. The function CALL
;;    applies an LZS-function to some arguments given also as LZS-objects. The
;;    interpreter is first used during macro expansion, but can also be used by
;;    compile-time evaluation of constant expressions and during activation of the
;;    MOP. The installation of interpreter functions for LZS-functions which body
;;    should not be interpreted is done by %annotate with the option
;;    "interpreter". The value given to this option must be one of the symbols in
;;    $system-function-table, which provides some functions of the compilation
;;    environment to the interpreter.
;;;  Documentation:
;;;  Notes:
;;    1. Up to now not all special forms are supported.
;;    2. $system-function-table should contain later all "basic" EuLisp functions. (What
;;                                                                                  basic means?)
;;    3. Global lexical variables are supported only if their initial value is a
;;    simple literal (symbol, character, string or number or lists/vectors of them) or
;;    if the value is set by a macro expansion before using it.
;;;  Requires:
;;;  Problems:
;;    1. What must be done to support MOP-activation during compile time?
;;    2. No module is initialized during macro expansion because the macro language is
;;    only a subset of EuLisp and it is not clear when a module should be initialized
;;    during compile time. Another problem is that there is no distinction between
;;    top-level forms needed during syntax expansion and forms needed during runtime
;;    only.
;;;  Authors: Ingo Mohr
;;;-----------------------------------------------------------------------------

#module lzs-eval

(import (eulisp1
         lzs
         eval-basic
         el2lzs-error
         (only (find-module)
               el2lzs-main)
         (only (mapcar
                mapc
                assoc
                rest
                first
                append
                char-code)
               common-lisp))
 syntax (eulisp1
         eval-basic
         (only (push)
               common-lisp))
 export (eval
         set-interpreter
         call)
 export (initialize-module))

;;;-----------------------------------------------------------------------------
;;; transforming literals to syntactic constructs
;;;-----------------------------------------------------------------------------
;;; make-syntactic transforms LZS-literals to objects of the compile time
;;; environment such that they can act as syntaxtic constructs. For example in
;;; the case of structured-literals of the LZS, which must be transformed to
;;; "normal" literals

(defgeneric make-syntactic (obj))

(defgeneric make-structured-literal-syntactic (obj)
  ;; this function is in some sense the counterpart of lzslit (el2lzs-rules)
  )

(defmethod make-syntactic ((obj <object>))
  obj)

(defmethod make-syntactic ((obj <sym>))
  ;; storing the retransformed value in ?syntactic fastens evaluation of symbols
  (?identifier obj))

(defmethod make-syntactic ((obj <structured-literal>))
  ;; storing the retransformed value in ?syntactic guarantees the eq-equality of
  ;; lists and vectors for eval
  ;; also eval works a bit faster by retransforming only once
  ;; the original value (available before transformation to the LZS) may be
  ;; stored in ?syntactic, but doing this only here avoids unnecessary references
  ;; to original structures if they are not used during macro expansion and it
  ;; makes eval more independent from other parts of the compiler
  (or (?syntactic obj)
      (setf (?syntactic obj)
            (make-structured-literal-syntactic (?value obj)))))

(defmethod make-structured-literal-syntactic (obj)
  ;; the default case
  (make-syntactic obj))

(defmethod make-structured-literal-syntactic ((obj <string>))
  obj)

(defmethod make-structured-literal-syntactic ((obj <vector>))
  (map #'make-syntactic obj))

(defun make-list-syntactic (l)
  (cond ((null? l) ())
        ((atom? l) (make-syntactic l))
        (t (cons (make-list-syntactic (car l))
                 (make-list-syntactic (cdr l))))))

(defmethod make-structured-literal-syntactic ((obj <pair>))
  (make-list-syntactic obj))

;;;-----------------------------------------------------------------------------
;;; eval
;;;-----------------------------------------------------------------------------

(defgeneric eval (lzsobj))

;;; the default case:
;;; return the object after transformation to a syntactic object
(defmethod eval ((obj <object>))
  (make-syntactic obj))

;;;-----------------------------------------------------------------------------
;;; function application
;;;-----------------------------------------------------------------------------

;;; Because of the optimization during transformation from EuLisp to LZS only
;;; <var-ref>, <fun> or <named-const> can appear as operator

(defgeneric call-eval-fun (fn args))

(defun call (fn args)
  (dynamic-let ((eval-fun fn)
                (eval-args args))
               (call-eval-fun fn args)))

(defmethod eval ((obj <app>))
  (call (eval (?function obj)) (mapcar #'eval (?arg-list obj))))

(defmethod call-eval-fun (obj args)
  (eval-error-expecting-function (make-syntactic obj)))

(defmethod call-eval-fun ((fun <simple-fun>) args)
  (let ((interpreter (?interpreter fun))
        (body (?body fun)))
    (cond (interpreter
           (apply interpreter args))
          ((null? (eq body ^unknown))
           (with-new-values (?var-list (?params fun))
                            (?rest (?params fun))
                            args
                            () ; the arguments are already evaluated
                            (eval body)))
          (t (eval-error-cannot-interpret-function fun)))))

(defmethod call-eval-fun ((fun <fun>) args)
  (let ((interpreter (?interpreter fun)))
    (if interpreter
        (apply interpreter args)
      (eval-error-cannot-interpret-function fun))))

;;;-----------------------------------------------------------------------------
;;; $system-function-table
;;;-----------------------------------------------------------------------------
;;; Is used by %annotate-handler 'set-interpreter' to install interpreter functions


(defconstant $system-function-table
  (declare-system-functions

    ;;--- special non-EuLisp-functions needed to implement the basics of EuLisp
    append ;; needed by quasiquote
    char-code             ;; needed to expand character literals

    ;;--- character
    character?
    as-lowercase
    as-uppercase

    ;;--- collection
    accumulate
    accumulate1
    any?
    collection?
    concatenate
    do
    element
    empty?
    fill
    map
    member
    reverse
    sequence?
    size

    ;;--- comparision
    eq
    eql
    equal
    =
    <
    max
    min

    ;;--- conversion
    ;;   convert
    ;;   converter

    ;;--- copy
    shallow-copy
    deep-copy

    ;;--- double-float
    double-float?

    ;;--- elementary-functions
    ;;   acos
    ;;   asin
    ;;   atan
    ;;   atan2
    ;;   cos
    ;;   sin
    ;;   tan
    ;;   cosh
    ;;   sinh
    ;;   tanh
    ;;   exp
    ;;   log
    ;;   log10
    ;;   pow
    ;;   sqrt
    ;;--- float, number
    float?
    ceiling
    floor
    round
    truncate
    ;;--- fpi
    int?
    ;;   ;--- formatted-io
    ;;   scan
    format
    ;;--- integer
    integer?
    even?
    odd?

    ;;--- list
    null?
    cons?
    atom?
    cons
    car
    cdr
    list

    ;;--- number
    number?
    +  ;; needed for :int :big
    - * /
    %
    gcd
    lcm
    abs
    zero?
    negate
    signum
    positive?
    negative?

    ;;--- stream
    stream?
    ;;   character-stream?
    ;;   standard-input-stream
    ;;   standard-output-stream
    ;;   standard-error-stream
    ;;   open
    close
    ;;   flush
    ;;   stream-position
    ;;   end-of-stream?
    ;;   input
    ;;   uninput
    ;;   output
    ;;   read-line
    ;;   prin
    print
    ;;   write
    ;;   newline

    ;;--- string
    string?
    ;;as-lowercase
    ;;as-uppercase
    binary<

    ;;--- symbol
    symbol?
    gensym
    make-symbol
    symbol-name
    symbol-exists?

    ;;--- table
    table?
    clear-table

    ;;--- vector
    vector?

    ))

(defun set-interpreter (lzs-fun annotate-key id)
  (let ((entry (assoc id $system-function-table)))
    (if entry
        (setf (?interpreter lzs-fun)
              (cdr entry))
      (eval-error-undefined-interpreter id))))

;;;-----------------------------------------------------------------------------
;;; named constants and variable references
;;;-----------------------------------------------------------------------------

(defgeneric get-value (var))

(defmethod get-value ((var <named-const>))
  (?value var))

(defmethod get-value ((var-ref <var-ref>))
  (get-value (?var var-ref)))

(defmethod get-value ((var <local-static>))
  (cdr (assoc var (dynamic variable-environment))))

(defmethod get-value ((var <global-static>))
  (let ((entry (assoc var *global-variables*)))
    (cond (entry (cdr entry))
          ((eq (?initial-value var) ^unknown)
           (eval-error-variable-without-value var)
           ())
          (t (?initial-value var)))))

(defgeneric set-value (var value))

(defmethod set-value ((var-ref <var-ref>) value)
  (set-value (?var var-ref) value))

(defmethod set-value ((var <local-static>) value)
  (setf (cdr (assoc var (dynamic variable-environment)))
        value))

(defmethod set-value ((var <global-static>) value)
  (let ((entry (assoc var *global-variables*)))
    (if entry
        (setf (cdr entry) value)
      (setq *global-variables*
            (cons (cons var value) *global-variables*)))
    value))

(defmethod eval ((obj <named-const>))
  (make-syntactic (?value obj)))

(defmethod eval ((obj <var-ref>))
  (get-value (?var obj)))

(defmethod eval ((obj <setq-form>))
  (set-value (?location obj) (eval (?form obj))))

;;;-----------------------------------------------------------------------------
;;; special forms
;;;-----------------------------------------------------------------------------

(defun eval-progn (forms)
  (let ((value ()))
    (mapc (lambda (form)
            (setq value (eval form)))
          forms)
    value))

(defmethod eval ((obj <progn-form>))
  (eval-progn (?form-list obj)))

(defmethod eval ((obj <if-form>))
  (if (eval (?pred obj))
      (eval (?then obj))
    (eval (?else obj))))

(defmethod eval ((obj <switch-form>))
  (eval-error-special-form-not-implemented ^switch)
  ())

(defmethod eval ((obj <labeled-form>))
  (eval-error-special-form-not-implemented ^labeled-form)
  ())

(defmethod eval ((obj <let*-form>))
  (with-new-values (?var-list obj) () (?init-list obj) #'eval
                   (eval (?body obj))))

(defmethod eval ((obj <labels-form>))
  (eval-error-special-form-not-implemented ^labels)
  ())

(defmethod eval ((obj <let/cc-form>))
  (eval-error-special-form-not-implemented ^let/cc)
  ())

(defmethod eval ((obj <tagbody-form>))
  (eval-error-special-form-not-implemented ^tagbody)
  ())

(defmethod eval ((obj <tagged-form>))
  (eval-error-special-form-not-implemented ^tagged-form)
  ())

(defmethod eval ((obj <mv-lambda>))
  (eval-error-special-form-not-implemented ^mv-lambda)
  ())

(defmethod eval ((obj <get-slot-value>))
  (eval-error-special-form-not-implemented ^%select)
  ())

(defmethod eval ((obj <set-slot-value>))
  (eval-error-special-form-not-implemented ^%setf-select)
  ())

;;;-----------------------------------------------------------------------------
;;; module initialization
;;;-----------------------------------------------------------------------------

(defun initialize-module (name)
  (call (?toplevel-forms (find-module name))
        ()))

;;;-----------------------------------------------------------------------------
#module-end
;;;-----------------------------------------------------------------------------
