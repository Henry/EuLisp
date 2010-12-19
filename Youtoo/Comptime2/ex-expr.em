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
;;; Title: expanding expressions into syntax nodes
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule ex-expr
  (syntax (_syntax-1
           _i-aux0
           _ex-aux0)
   import (i-all
           p-env
           sx-node
           sx-obj
           ex-import
           ex-syntax
           ex-direct
           cg-dld
           sx-write)
   export (macroexpand-1
           macroexpand
           expand-syntax-1
           expand-syntax
           complete-lambda-node
           filter-vars
           filter-init-forms
           *nil*
           protect-tilde
           protect-newline
           protect-backslash
           protect-doublequote))

;;;-----------------------------------------------------------------------------
;;; Expression expander
;;;-----------------------------------------------------------------------------
(defconstant get-expr-expander (make-access-table))

(defun expand-expr (expr expr-context . envs)
  (expr-expander expr expr-context (and envs (car envs)) expr-expander))

(defun expand-exprs (exprs . envs)
  (let ((env (and envs (car envs))))
    (map1-list (lambda (expr)
                 (expr-expander expr exprs env expr-expander))
               exprs)))

(defun install-expr-expander (key fun)
  (let ((x (get-expr-expander key)))
    (and x
         (ct-warning () "redefinition of expander ~a" key))
    ((setter get-expr-expander) key fun)))

(defun expr-expander (x expr-context env e)
  (notify0 "    Expanding ~a" x)
  (let ((expander
         (cond
           ((symbol? x)
            (get-id-expander x)) ; simple identifier
           ((syntax-obj? x)
            (lambda (x expr-context env e) x)) ; already expanded
           ((null? (cons? x))
            (lambda (x expr-context env e) ; literal constant
              (make <literal-const> value: x)))
           ((symbol? (car x))
            (or (get-macro-expander (car x))
                (get-expr-expander (car x))
                (get-appl-expander (car x))))
           (t (get-appl-expander (car x))))))
    (expander x expr-context env e)))

;;;-----------------------------------------------------------------------------
;;; Macro expander
;;;-----------------------------------------------------------------------------
(defun get-macro-expander (key)
  (let ((binding (get-syntax-binding key)))
    (and binding
         (let ((macro-fun (as-dynamic-binding binding)))
           (and macro-fun
                (lambda (x expr-context env e)
                  (with-ct-handler
                   (protect-tilde
                    (fmt "bad macro expansion of ~a in ~s"
                         (cons key (cdr x)) expr-context))
                   macro-fun
                   (notify0 "APPLY MACRO: ~a" (cons key (cdr x)))
                   (let ((macro-expanded-form
                          (progn (setq *pass* 'execute)
                                 (apply macro-fun (cdr x)))))
                     (notify0 "RESULT: ~a" macro-expanded-form)
                     (e macro-expanded-form expr-context env e)))))))))

(defun macroexpand-1 (expr)
  (if (cons? expr)
      (let ((binding (get-syntax-binding (car expr))))
        (if binding
            (let ((macro-fun (as-dynamic-binding binding)))
              (if macro-fun
                  (progn (setq *pass* 'execute)
                         (apply macro-fun (cdr expr)))
                (error <condition>
                       (fmt "macroexpand-1 cannot find dynamic binding ~a for syntax binding ~a"
                            binding (car expr)))))
          (error <condition>
                 (fmt "macroexpand-1: cannot find syntax binding ~a"
                      (car expr)))))
    (error <condition>
           (fmt "macroexpand-1: expression ~a is not a cons" expr))))

(defun expand-syntax-1 (expr)
  (if (cons? expr)
      (let ((binding (get-syntax-binding (car expr))))
        (if binding
            (let ((macro-fun (as-dynamic-binding binding)))
              (if macro-fun
                  (progn (setq *pass* 'execute)
                         (apply macro-fun (cdr expr)))
                (error <condition>
                       (fmt "expand-syntax-1 cannot find dynamic binding ~a for syntax binding ~a"
                            binding (car expr)))))
          (error <condition>
                 (fmt "expand-syntax-1: cannot find syntax binding ~a"
                      (car expr)))))
    (error <condition>
           (fmt "expand-syntax-1: expression ~a is not a cons" expr))))

(defun macroexpanded? (expr)
  (if (cons? expr)
      (let ((binding (get-syntax-binding (car expr))))
        (if binding
            (let ((macro-fun (as-dynamic-binding binding)))
              (if macro-fun
                  (progn (setq *pass* 'execute)
                         (apply macro-fun (cdr expr)))
                ()))
          ()))
    ()))

(defun macroexpand (expr)
  (if (cons? expr)
      (let ((eexpr (macroexpanded? expr)))
        (if eexpr
            (macroexpand eexpr)
          (if (and (cons? expr)
                   (not (eq (car expr) 'quote))
                   (not (eq (car expr) 'quasiquote)))
              (cons (macroexpand (car expr)) (macroexpand (cdr expr)))
            expr)))
    expr))

(defun expand-syntax (expr)
  (if (cons? expr)
      (let ((eexpr (macroexpanded? expr)))
        (if eexpr
            (expand-syntax eexpr)
          (if (and (cons? expr)
                   (not (eq (car expr) 'quote))
                   (not (eq (car expr) 'quasiquote)))
              (cons (expand-syntax (car expr)) (expand-syntax (cdr expr)))
            expr)))
    expr))

(defun protect-tilde (str)
  (let ((i (member1-string #\~ str))
        (n (string-size str)))
    (if i
        (if (= i 0)
            (if (= n 1) "~~"
              (string-append "~~"
                             (protect-tilde (substring str (+ i 1) ()))))
          (if (= i (- n 1))
              (string-append (substring str () i) "~~")
            (string-append
             (substring str () i)
             (string-append "~~"
                            (protect-tilde (substring str (+ i 1) ()))))))
      str)))

(defun protect-doublequote (str)
  (let ((i (member1-string #\" str))
        (n (string-size str)))
    (if i
        (if (= i 0)
            (if (= n 1) "\\\""
              (string-append "\\\""
                             (protect-doublequote (substring str (+ i 1) ()))))
          (if (= i (- n 1))
              (string-append (substring str () i) "\\\"")
            (string-append
             (substring str () i)
             (string-append "\\\""
                            (protect-doublequote (substring str (+ i 1) ()))))))
      str)))

(defun protect-backslash (str)
  (let ((i (member1-string #\\\ str))
        (n (string-size str)))
    (if i
        (if (= i 0)
            (if (= n 1) "\\\\"
              (string-append "\\\\"
                             (protect-backslash (substring str (+ i 1) ()))))
          (if (= i (- n 1))
              (string-append (substring str () i) "\\\\")
            (string-append
             (substring str () i)
             (string-append "\\\\"
                            (protect-backslash (substring str (+ i 1) ()))))))
      str)))

(defun protect-newline (str)
  ;    (let ((i (member1-string #\\n str))
  (let ((i (member1-string #\\n str))
        (n (string-size str)))
    (if i
        (if (= i 0)
            (if (= n 1) "\\n"
              (string-append "\\n"
                             (protect-newline (substring str (+ i 1) ()))))
          (if (= i (- n 1))
              (string-append (substring str () i) "\\n")
            (string-append
             (substring str () i)
             (string-append "\\n"
                            (protect-newline (substring str (+ i 1) ()))))))
      str)))

;;;-----------------------------------------------------------------------------
;;; Identifier expander
;;;-----------------------------------------------------------------------------
(defun get-id-expander (key)
  (lambda (x expr-context env e)
    (let ((node (or (get-local-static-binding x env)
                    (get-lexical-binding x)
                    (get-t-node x)
                    (get-keyword-node x)
                    (ct-serious-warning
                     (make-dummy-binding x)
                     "no lexical binding ~a available in ~s" x expr-context))))
      (if (binding? node)
          (check-id-binding node)
        node))))

(defun check-id-binding (binding)
  (register-binding-ref binding)
  (let ((obj (binding-obj? binding)))
    (if (or (opencoding? obj)  ; opencoding and ff have to be "boxed"
            (get-binding-info binding 'opencoding)
            (get-binding-info binding 'ff))
        (box-binding binding)
      (progn
        (and (fun? obj)               ; fun is used as object?
             (fun-has-unknown-appls! obj t))
        binding))))

;;;-----------------------------------------------------------------------------
;;; Nil and t
;;;-----------------------------------------------------------------------------
;; This is an internal constant `nil' used during compilation
;; (eq 'nil '*nil*) => (), (eq 'nil '()) => t
(defconstant *nil* (make <literal-const> value: ()))

;; This is the constant that 't refers to....
(defconstant *t* (make <literal-const> value: 't))

;; ... using the mapping function:
(defun get-t-node (x)
  (if (eq x 't)
      *t*
    ()))

;;;-----------------------------------------------------------------------------
;;; Keywords
;;;-----------------------------------------------------------------------------
(defun get-keyword-node (x)
  (and (keyword? x)
       (make <literal-const> value: x)))

;;;-----------------------------------------------------------------------------
;;; Box opencoding and ff bindings (reverse teta-conversion)
;;;-----------------------------------------------------------------------------
(defun box-binding (binding)
  (notify0 "box binding ~a" (binding-local-name? binding))
  (let* ((arity (get-binding-info binding 'arity))
         (binding-name (binding-local-name? binding))
         (args (dummy-args arity)))
    (expand-expr `(lambda ,args (,binding-name ,@args)) binding)))

(defun dummy-args (arity)
  (labels
   ((loop (res)
          (if (< 0 arity)
              (progn
                (setq arity (- arity 1))
                (loop (cons (gensym) res)))
            res)))
   (if (negative? arity)  ; rest args?
       (progn
         (setq arity (- (abs arity) 1))
         (loop (gensym)))
     (loop ()))))

(defun rest-args? (args)
  (or (symbol? args)
      (and (cons? args)
           (not (proper-list? args)))))

(defun lambda-rest-args? (op)
  (if (not (cons? op))
      ()
    (let ((lambda-type (car op)))
      (cond ((or (eq lambda-type 'lambda)
                 (eq lambda-type 'inlined-lambda))
             (rest-args? (cadr op)))
            ((eq lambda-type 'named-lambda)
             (rest-args? (caddr op)))
            (t ())))))

;;;-----------------------------------------------------------------------------
;;; Application expander
;;  (+ - * / % = < <= > >= can be simplified in some cases into
;;  fpi-binary or fpi-inc/inc-dec calls assuming a trap to the gf from the
;;  correspondin vm instruction)
;;;-----------------------------------------------------------------------------
(defun get-appl-expander (key)
  (lambda (x expr-context env e)
    (let* ((op (car x))
           (params (cdr x))
           (nargs (list-size params))
           (op
            ;; This is a workaround for a code generation bug.
            ;; lambda expressions with rest arguments that are in the
            ;; operator position are compiled incorrectly when they are
            ;; inlined, because all of their arguments are pushed onto
            ;; the stack separately, but they expect that their last
            ;; argument will be a list containing the rest of the
            ;; arguments.  In other words,
            ;;     ((lambda args args) 1 2 3 4) => 4
            ;;     ((lambda (n . rest) (list n rest) 1 2 3 4) => (3 4)
            ;; So, we wrap a (lambda () ...) around the original lambda
            ;; expression, so that it isn't in the operator position
            ;; any more.
            (cond ((lambda-rest-args? op)
                   (notify0 "  wrapping lambda in operator position: ~s" op)
                   `((lambda () ,op)))
                  (t op))))
      (cond ((and (member op '(+ - * / %)) ;   <=))
                  (< 1 nargs)
                  (eq (binding-module? (get-lexical-binding op)) 'number))
             (e (unfold-rest-arg-appl op params) expr-context env e))
            ((and (eq op '>)
                  (= 2 nargs)
                  (eq (binding-module? (get-lexical-binding op)) 'compare))
             (e `(fpi-binary< ,@(reverse params)) expr-context env e))
            ((and (or (eq op '=) (eq op '<))
                  (= 2 nargs)
                  (eq (binding-module? (get-lexical-binding op)) 'compare))
             (e (unfold-rest-arg-appl op params) expr-context env e))
            ((and (eq op '>=)
                  (= 2 nargs)
                  (eq (binding-module? (get-lexical-binding op)) 'compare))
             (e `(if (fpi-binary< ,@(reverse params))
                     t
                   (fpi-binary= ,@params))
                expr-context env e))
            ((and (eq op '<=)
                  (= 2 nargs)
                  (eq (binding-module? (get-lexical-binding op)) 'compare))
             (e `(if (fpi-binary< ,@params)
                     t
                   (fpi-binary= ,@params)) expr-context env e))
            ((or (and (eq op 'fpi-binary+)
                      (eq (binding-module? (get-lexical-binding op)) 'boot1))
                 (and (eq op 'binary+)
                      (eq (binding-module? (get-lexical-binding op)) 'number)))
             (let ((arg1 (car params))
                   (arg2 (cadr params)))
               (cond ((and (number? arg1)
                           (number? arg2))
                      ;; partial evaluation
                      (e (+ arg1 arg2) expr-context env e))
                     ((and (integer? arg1)
                           (= arg1 1))
                      (e `(inc ,arg2) expr-context env e))
                     ((and (integer? arg2)
                           (= arg2 1))
                      (e `(inc ,arg1) expr-context env e))
                     ((and (integer? arg2)
                           (= arg2 -1))
                      (e `(dec ,arg1) expr-context env e))
                     (t
                      (default-appl-expander
                       op params nargs expr-context env)))))
            ((or (and (eq op 'fpi-binary-)
                      (eq (binding-module? (get-lexical-binding op)) 'boot1))
                 (and (eq op 'binary-)
                      (eq (binding-module? (get-lexical-binding op)) 'number)))
             (let ((arg1 (car params))
                   (arg2 (cadr params)))
               (cond ((and (number? arg1)
                           (number? arg2))
                      ;; partial evaluation
                      (e (- arg1 arg2) expr-context env e))
                     ;; x - 1
                     ((and (integer? arg2)
                           (= arg2 1))
                      (e `(dec ,arg1) expr-context env e))
                     ;; x - -1
                     ((and (integer? arg2)
                           (= arg2 -1))
                      (e `(inc ,arg1) expr-context env e))
                     (t
                      (default-appl-expander
                       op params nargs expr-context env)))))
            ((or (and (eq op 'fpi-binary=)
                      (eq (binding-module? (get-lexical-binding op)) 'boot1))
                 (and (eq op 'binary=)
                      (eq (binding-module? (get-lexical-binding op)) 'compare)))
             (let ((arg1 (car params))
                   (arg2 (cadr params)))
               (cond ((and (number? arg1)
                           (number? arg2))
                      ;; partial evaluation
                      (e (= arg1 arg2) expr-context env e))
                     ;; x = 0
                     ((and (integer? arg2)
                           (= arg2 0))
                      (e `(fpi-zero? ,arg1) expr-context env e))
                     ;; 0 = x
                     ((and (integer? arg1)
                           (= arg1 0))
                      (e `(fpi-zero? ,arg2) expr-context env e))
                     (t
                      (default-appl-expander
                       op params nargs expr-context env)))))
            ((and (cons? op) (eq (car op) 'lambda))
             (e `((inlined-lambda ,@(cdr op)) ,@params) expr-context env e))
            (t
             (default-appl-expander op params nargs expr-context env))))))

(defun default-appl-expander (op params nargs expr-context env)
  (let* ((fun (expand-fun-form op expr-context env))
         (args (expand-exprs params env))
         (lifted-appl (lift-appl (cons fun args) () () env))
         (new-exprs (car lifted-appl))
         (new-fun (car new-exprs))
         (new-args (cdr new-exprs))
         (new-vars (car (cdr lifted-appl)))
         (appl (make <appl> fun: new-fun args: new-args)))
    (check-appl appl new-fun expr-context)
    (if (null? new-vars)
        appl
      (progn
        (do1-list (lambda (var)
                    (var-used! var (+ (var-used? var) 1)))
                  new-vars)
        (make-let* new-vars appl)))))

(defun unfold-rest-arg-appl (op args)
  (let ((binary-op (concatenate 'fpi-binary op)))
    (labels
     ((loop (l)
            (if (null? (cdr l))
                (car l)
              (list binary-op (loop (cdr l)) (car l)))))
     (loop (reverse-list args)))))

(defun expand-fun-form (x expr-context env)
  ;; Like id expander, but binding is not checked
  (if (atom? x)
      (let ((binding (or (get-local-static-binding x env)
                         (get-lexical-binding x)
                         (ct-serious-warning
                          (make-dummy-binding x)
                          "no lexical binding ~a available in ~s"
                          x expr-context))))
        (register-binding-ref binding)
        binding)
    (if (and (eq (car x) 'setter) (symbol? (cadr x))
             (null? *interpreter*))
        (or (get-inlined-setter-binding x env)
            (expand-expr x expr-context env))
      (expand-expr x expr-context env))))

;;;-----------------------------------------------------------------------------
;;; Lift applications
;;;-----------------------------------------------------------------------------
(defun lift-appl (exprs new-exprs new-vars env)
  (if (null? exprs)
      (list (reverse new-exprs) new-vars)
    (let ((expr (car exprs))
          (rest (cdr exprs)))
      (cond ((let*? expr)
             ;; -------------------------------------------------
             ;; local static variables are collected
             ;; -------------------------------------------------
             (lift-appl (cons (fun-body? expr) rest)
                        new-exprs
                        (append new-vars (fun-args? expr))
                        env))
            ;; -------------------------------------------------
            ;; simple values stay where they are
            ;; -------------------------------------------------
            ((or (literal-const? expr) (binding? expr) (fun? expr))
             (lift-appl rest (cons expr new-exprs) new-vars env))
            ;; -------------------------------------------------
            ;; other argument expressions are lifted
            ;; -------------------------------------------------
            (t (let ((tmp-vars (expand-local-static-vars
                                (list (gensym)) (list expr) env)))
                 (lift-appl rest
                            (append (map1-list var-binding? tmp-vars)
                                    new-exprs)
                            (append new-vars tmp-vars)
                            env)))))))

;;;-----------------------------------------------------------------------------
;;; Match function and function application?
;;;-----------------------------------------------------------------------------
(defgeneric check-appl (appl op expr-context))

(defmethod check-appl ((appl <appl>) (op <object>) expr-context)
  (if (function? op)
      (ct-serious-warning () "macro binding ~a should be in syntax import in ~s"
                          (binding-local-name? (appl-fun? appl)) expr-context)
    (ct-serious-warning () "no applicable object ~a in ~s" op expr-context)))

(defmethod check-appl ((appl <appl>) (op <fun>) expr-context)
  (check-appl-arity appl op (local-name? op))
  (fun-appls! op (cons appl (fun-appls? op))))

(defmethod check-appl ((appl <appl>) (op <interface-binding>) expr-context)
  ;; e.g. could check arity, when written in interface file
  ())

(defmethod check-appl ((appl <appl>) (op <binding>) expr-context)
  (and (binding-obj? op)              ; no check with dummy binding
       (check-appl appl (binding-obj? op) expr-context)))

(defmethod check-appl ((appl <appl>) (op <var>) expr-context)
  ;; is ok
  ())

(defmethod check-appl ((appl <appl>) (op <named-const>) expr-context)
  ;; is ok
  ())

;;;-----------------------------------------------------------------------------
;;; Call-next-method expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'call-next-method
 (lambda (x expr-context env e)
   (let ((vars (fun-args? (dynamic *encl-lambda*))))
     (do1-list (lambda (var) (var-used! var (+ (var-used? var) 1))) vars)
     (make <call-next-method>))))

;;;-----------------------------------------------------------------------------
;;; Check if lambda is called with right number of arguments
;;;-----------------------------------------------------------------------------
(defun check-appl-arity (appl fun . name)
  (let* ((arity (fun-arity? fun))
         (n (list-size (appl-args? appl)))
         (fun-name (or (and name (car name)) fun)))
    (if (< arity 0)                   ; rest args?
        (or (null? (< (+ n 1) arity))
            (ct-serious-warning () "too few arguments calling ~a" fun-name))
      (and (null? (= arity n))
           (if (< arity n)
               (ct-serious-warning () "too many arguments calling ~a"
                                   fun-name)
             (ct-serious-warning () "too few arguments calling ~a"
                                 fun-name))))))

;;;-----------------------------------------------------------------------------
;;; Install if expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'if
 (lambda (x expr-context env e)
   (with-ct-handler
    (fmt "bad if syntax ~a in ~s" x expr-context) x
    (let* ((pred-expr (cadr x))
           (then-expr (caddr x))
           (else-expr1 (cdddr x))
           (else-expr (and else-expr1 (car else-expr1)))
           (pred-node (expand-expr pred-expr expr-context env))
           (then-node (expand-expr then-expr expr-context env))
           (else-node (expand-expr else-expr expr-context env)))
      (if (or *no-else* else-expr1)
          (if (cdddr (cdr x))
              (ct-serious-warning
               ()
               "bad if syntax (if ~a ...) in ~s" pred-expr expr-context)
            ())
        (ct-warning
         ()
         "missing else branch in (if ~a ...) in ~s" pred-expr expr-context))
      (lift-if pred-node then-node else-node env)))))

(defgeneric lift-if (pred-obj then-e else-e env))

;;;-----------------------------------------------------------------------------
;;; Simple test expressions are not lifted (with some partial evaluation)
;;;-----------------------------------------------------------------------------
(defmethod lift-if ((pred-e <binding>) then-e else-e env)
  (make <if> pred: pred-e then: then-e else: else-e))

(defmethod lift-if ((pred-e <literal-const>) then-e else-e env)
  (if (const-value? pred-e) then-e else-e))

(defmethod lift-if ((pred-e <lambda>) then-e else-e env)
  then-e)

;;;-----------------------------------------------------------------------------
;;; Collect local static variables
;;;-----------------------------------------------------------------------------
(defmethod lift-if ((pred-e <let*>) then-e else-e env)
  (let ((lifted-body (lift-if (fun-body? pred-e) then-e else-e env)))
    (if (let*? lifted-body)
        (let ((new-vars (append (fun-args? pred-e)
                                (fun-args? lifted-body))))
          (fun-args! pred-e new-vars)
          (fun-body! pred-e (fun-body? lifted-body)))
      (fun-body! pred-e lifted-body)))
  pred-e)

;;;-----------------------------------------------------------------------------
;;; Other test expressions are lifted
;;;-----------------------------------------------------------------------------
(defmethod lift-if ((pred-e <syntax-obj>) then-e else-e env)
  (let ((var-name (gensym)))
    (expand-expr `(let ((,var-name ,pred-e))
                    (if ,var-name ,then-e ,else-e)) "unknown context" env)))

;;;-----------------------------------------------------------------------------
;;; Install progn expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'progn
 (lambda (x expr-context env e)
   (cond ((null? (cdr x))                     ; no empty progn?
          *nil*)
         ((null? (cdr (cdr x)))               ; no one-form progn?
          (expand-expr (car (cdr x)) expr-context env))
         (t
          (let ((exprs ())
                (last-expr ())
                (dummy-name (gensym)))
            (do1-list-last-special
             (lambda (x) (setq exprs (cons (list dummy-name x) exprs)))
             (lambda (x) (setq last-expr x))
             (cdr x))
            (expand-expr `(let* ,(reverse-list exprs) ,last-expr)
                         expr-context env))))))

;;;-----------------------------------------------------------------------------
;;; Install return expander
;;;-----------------------------------------------------------------------------
;  (install-expr-expander 'return
;    (lambda (x expr-context env e)
;      (make <return> form: (expand-expr (cadr x) expr-context env))))

;;;-----------------------------------------------------------------------------
;;; Install quote expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'quote
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad quote syntax" x
    (make <literal-const> value: (cadr x)))))

;  (defun trailing-colon? (sym)
;    (let ((str (symbol-name sym)))
;      (eq (string-ref str (- (string-size str) 1)) #\:)))

;;;-----------------------------------------------------------------------------
;;; Install quasiquote expander (sometimes called backquote)
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'quasiquote
 (lambda (xx expr-context env e)
   (with-ct-handler
    "bad quasiquote syntax" xx
    ;; not tail recursive (is it possible?)
    (let ((x (car (cdr xx))))
      (if (atom? x)
          (e (list 'quote x) expr-context env e)
        (if (eq (car x) 'unquote)
            (e (car (cdr x)) expr-context env e)
          (labels
           ((loop (xx)
                  (if (atom? xx)
                      (list 'quasiquote xx)
                    (if (eq (car xx) 'unquote)
                        (list 'quasiquote xx)
                      (let ((x1 (car xx)))
                        (if (cons? x1)
                            (let ((x11 (car x1)))
                              (if (eq x11 'unquote)
                                  (list 'cons (car (cdr x1))
                                        (loop (cdr xx)))
                                (if (eq x11 'unquote-splicing)
                                    (let* ((l1 (car (cdr x1)))
                                           (l2 (cdr xx))
                                           (l3 (loop l2)))
                                      (if (null? l2) l1
                                        (list 'append l1 l3)))
                                  (list 'cons (list 'quasiquote x1)
                                        (loop (cdr xx))))))
                          (list 'cons (list 'quasiquote x1)
                                (loop (cdr xx)))))))))
           (e (loop x) expr-context env e))))))))

;;;-----------------------------------------------------------------------------
;;; Install setq expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'setq
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad setq syntax" x
    (dynamic-let
     ((tail-pos? ()))
     (let* ((name (cadr x))
            (binding (or (get-local-static-binding name env)
                         (get-lexical-binding name)
                         (ct-serious-warning
                          (make-dummy-binding name)
                          "no binding ~a available in ~s" name expr-context)))
            (obj (binding-obj? binding))
            (set-immutable (cdr (cdr (cdr x)))))
       (register-binding-ref binding)
       (and (binding-immutable? binding)
            (null? set-immutable)
            (ct-serious-warning
             ()
             "immutable binding ~a cannot be modified in ~s"
             name expr-context))
       (lift-setq binding (expand-expr (caddr x) expr-context env) env))))))

(defgeneric lift-setq (binding value env))

(defmethod lift-setq ((binding <binding>) (value <lambda>) env)
  (make-setq binding value))

(defmethod lift-setq ((binding <binding>) (value <literal-const>) env)
  (make-setq binding value))

(defmethod lift-setq ((binding <binding>) (value <binding>) env)
  (make-setq binding value))

(defmethod lift-setq ((binding <binding>) (value <syntax-obj>) env)
  (let* ((vars (expand-local-static-vars (list (gensym)) (list value) env))
         (lifted-body (lift-setq binding (var-binding? (car vars)) env)))
    (do1-list (lambda (var) (var-used! var (+ (var-used? var) 1))) vars)
    (make-let* vars lifted-body)))

(defmethod lift-setq ((binding <binding>) (value <let*>) env)
  (let ((lifted-body (lift-setq binding (fun-body? value) env))
        (args (fun-args? value)))
    (if (let*? lifted-body)
        (let ((new-args (fun-args? lifted-body)))
          (do1-list (lambda (var) (var-used! var (+ (var-used? var) 1)))
                    new-args)
          (make-let* (append args new-args) (fun-body? lifted-body)))
      (make-let* args lifted-body))))

;;;-----------------------------------------------------------------------------
;;; Install lambda expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'lambda
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad lambda syntax" x
    (let ((node (make-fun <lambda> 'anonymous
                          (get-lambda-params x) (get-lambda-body x)
                          t)))      ; has unknown applications
      (complete-lambda-node node expr-context env)
      node))))

(install-expr-expander
 'named-lambda
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad named lambda syntax" x
    (let* ((name (make-symbol (fmt "~a" (car (cdr x)))))
           (node (make-fun <lambda> (list name)
                           (get-params x) (get-body x)
                           t)))     ; has unknown applications
      (complete-lambda-node node expr-context env)
      node))))

(defun complete-lambda-node (fun expr-context . envs)
  (let ((env (and envs (car envs)))
        (encl-lambda
         (if (lambda-inlined? fun) (dynamic *encl-lambda*) fun)))
    (fun-range-and-domain! fun (compute-range-and-domain fun))
    (dynamic-let ((*encl-lambda* encl-lambda))
                 (let* ((args (expand-local-static-vars (fun-args? fun) () env))
                        (new-env (add-local-static-bindings
                                  (map1-list var-binding? args) env)))
                   (fun-args! fun args)
                   (and (lambda-inlined? fun)
                        (register-delegated-vars args))
                   (do1-list (lambda (var)
                               (local-static-var-lambda! var fun)) args)
                   (fun-body! fun (expand-expr (fun-body? fun)
                                               expr-context
                                               new-env))))))

(defun compute-range-and-domain (fun)
  ;;(let* ((arity (fun-arity? fun))
  ;;       (v (make <vector>
  ;;              size: (+ (abs arity) 1)
  ;;              fill-value: (expand-expr '<object>))))
  ;;  (and (< arity 0)
  ;;     ((setter vector-ref) v (abs arity) (expand-expr '<list>)))
  ;;  (fun-range-and-domain! fun v))
  )

;;;-----------------------------------------------------------------------------
;;; Install inlined-lambda expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'inlined-lambda
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad lambda syntax" x
    (let ((node (make-fun <lambda> 'anonymous
                          (get-lambda-params x) (get-lambda-body x)
                          t)))      ; has unknown applications
      (lambda-inlined! node t)
      (complete-lambda-node node expr-context env)
      node))))

;;;-----------------------------------------------------------------------------
;;; Install opencoded-lambda expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'opencoded-lambda
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad opencoded-lambda syntax" x
    (let ((node (make-fun <opencoding> 'anonymous
                          (get-lambda-params x) (get-lambda-body x)
                          t)))      ; unknown applications
      (fun-body! node (cdr (fun-body? node))) ; remove default progn
      node))))

;;;-----------------------------------------------------------------------------
;;; Install let expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'let
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad let syntax" x
    (let ((decls (car (cdr x)))
          (body (cdr (cdr x))))
      (cond ((null? decls)                 ; empty let
             (e `(progn ,@body) expr-context env e))
            ((cons? decls)                ; simple let
             (if (= (list-size decls) 1)  ; let -> let*
                 (e `(let* ,decls ,@body) expr-context env e)
               (let* ((args (filter-vars decls))
                      (init-forms (filter-init-forms decls)))
                 (e `((inlined-lambda ,args ,@body) ,@init-forms)
                    expr-context env e))))
            ((symbol? decls)              ; named let
             (let ((named-let-clauses (car body))
                   (named-let-body (cdr body)))
               (e `(labels
                    ((,decls (,@(map1-list car named-let-clauses))
                             ,@named-let-body))
                    (,decls ,@(map1-list (lambda (x) (car (cdr x)))
                                         named-let-clauses)))
                  expr-context env e))))))))

;;;-----------------------------------------------------------------------------
;;; Install let* expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'let*
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad let* syntax" x
    (cond ((null? (cadr x))
           (expand-expr (list 'progn (caddr x)) expr-context env))
          ((cons? (cadr x))
           (let* ((inits (filter-init-forms (cadr x)))
                  (vars-and-env (expand-local-static-vars*
                                 (filter-vars (cadr x)) inits env))
                  (vars (car vars-and-env))
                  (new-env (cdr vars-and-env))
                  (body (expand-expr `(progn ,@(cdr (cdr x)))
                                     expr-context new-env))
                  (new-vars (lift-let*-vars vars)))
             (if (let*? body)
                 (make-let* (append new-vars (fun-args? body))
                            (fun-body? body))
               (make-let* new-vars body))))
          (t (error <condition> ""))))))

(defun lift-let*-vars (vars)
  (labels
   ((loop (vars new-vars)
          (if (null? vars)
              (reverse new-vars)
            (let ((init-form (var-value? (car vars))))
              (cond
                ((let*? init-form)
                 (var-value! (car vars) (fun-body? init-form))
                 (loop (cdr vars) (cons (car vars)
                                        (append (reverse (fun-args? init-form))
                                                new-vars))))
                (t
                 (var-value! (car vars) init-form)
                 (loop (cdr vars) (cons (car vars) new-vars))))))))
   (loop vars ())))

;;;-----------------------------------------------------------------------------
;;; Install labels expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'labels
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad labels syntax" x
    (let ((decls (car (cdr x)))
          (body (cdr (cdr x))))
      (expand-expr `(let ,(labelsvar decls)
                      ,@(labelssetq decls)
                      ,@body) expr-context env)))))

(defun labelsvar (decls)
  (and decls
       (cons (list (car (car decls)) ())
             (labelsvar (cdr decls)))))

(defun labelssetq (decls)
  (and decls
       (cons `(setq ,(car (car decls))
                    (lambda ,(car (cdr (car decls)))
                      ,@(cdr (cdr (car decls)))))
             (labelssetq (cdr decls)))))

;;;-----------------------------------------------------------------------------
;;; Install letfuns expander
;;;-----------------------------------------------------------------------------
(install-expr-expander
 'letfuns
 (lambda (x expr-context env e)
   (with-ct-handler
    "bad letfuns syntax" x
    (let ((decls (car (cdr x)))
          (body (cdr (cdr x))))
      (expand-expr `(let ,(letfunsvar decls)
                      ,@(letfunssetq decls)
                      ,@body) expr-context env)))))

(defun letfunsvar (decls)
  (and decls
       (cons (list (car (car decls)) ())
             (letfunsvar (cdr decls)))))

(defun letfunssetq (decls)
  (and decls
       (cons `(setq ,(car (car decls))
                    (lambda ,(car (cdr (car decls)))
                      ,@(cdr (cdr (car decls)))))
             (letfunssetq (cdr decls)))))

;;;-----------------------------------------------------------------------------
;;; Expand local-static-vars
;;;-----------------------------------------------------------------------------
(defun expand-local-static-vars (var-names init-forms env)
  (let ((true-var-names (as-proper-list var-names)))
    (and (null? init-forms)
         (setq init-forms (map1-list null? true-var-names)))
    (map2-list (lambda (var init-form)
                 (make-local-static-var
                  var
                  (expand-expr init-form init-forms env)))
               true-var-names init-forms)))

(defun expand-local-static-vars* (var-names init-forms env)
  (labels
   ((loop (vars forms env)
          (if (null? vars) env
            (let* ((var (car vars))
                   (init-form (car forms))
                   (new-env (add-local-static-bindings
                             (list (var-binding? var)) env)))
              (var-value! var (expand-expr init-form init-forms env))
              (loop (cdr vars) (cdr forms) new-env)))))
   (let* ((vars (map1-list make-local-static-var (as-proper-list var-names)))
          (new-env (if (null? init-forms) env
                     (loop vars init-forms env))))
     (cons vars new-env))))

;;;-----------------------------------------------------------------------------
;;; Filter vars/init-forms out of a list like '((a u) b (c w))
;;;-----------------------------------------------------------------------------
(defun filter-vars (l)
  (map1-list (lambda (x) (if (cons? x) (car x) x)) l))

(defun filter-init-forms (l)
  (map1-list (lambda (x) (if (cons? x) (cadr x) ())) l))

;;;-----------------------------------------------------------------------------
)  ;; End of module ex-expr
;;;-----------------------------------------------------------------------------
