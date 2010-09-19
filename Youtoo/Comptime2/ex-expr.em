;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: expanding expressions into syntax nodes
;;;-----------------------------------------------------------------------------

(defmodule ex-expr
  (syntax (_macros
           _i-aux0
           _ex-aux0)
   import (i-all
           p-env
           sx-node
           sx-obj
           ex-import
           ex-syntax
           ex-direct
           cg-dld)
   export (expand-expr
           expand-exprs
           get-macro-expander
           complete-lambda-node
           filter-vars
           filter-init-forms
           *nil*
           protect-tilde
           protect-newline
           protect-backslash
           protect-doublequote))

;;;-----------------------------------------------------------------------------
;;; EXPRESSION expander
;;;-----------------------------------------------------------------------------
(defconstant get-expr-expander (make-access-table))

(defun expand-expr (x . envs)
  (expr-expander x (and envs (car envs)) expr-expander))

(defun expand-exprs (x . envs)
  (let ((env (and envs (car envs))))
    (map1-list (lambda (expr)
                 (expr-expander expr env expr-expander))
               x)))
(defun install-expr-expander (key fun)
  (let ((x (get-expr-expander key)))
    (and x
         (ct-warning () "redefinition of expander ~a" key))
    ((setter get-expr-expander) key fun)))
(defun expr-expander (x env e)
  (notify0 "    Expanding ~a" x)
  (let ((expander
         (cond
           ((symbol? x) (get-id-expander x)) ; simple identifier
           ((syntax-obj? x) (lambda (x env e) x)) ; already expanded
           ((null? (cons? x)) (lambda (x env e) ; literal constant
                                (make <literal-const> value: x)))
           ((symbol? (car x)) (or (get-macro-expander (car x))
                                  (get-expr-expander (car x))
                                  (get-appl-expander (car x))))
           (t (get-appl-expander (car x))))))
    (expander x env e)))

;;;-----------------------------------------------------------------------------
;;; MACRO expander
;;;-----------------------------------------------------------------------------
(defun get-macro-expander (key)
  (let ((binding (get-syntax-binding key)))
    (and binding
         (let ((macro-fun (as-dynamic-binding binding)))
           (and macro-fun
                (lambda (x env e)
                  (with-ct-handler (protect-tilde
                                    (fmt "bad macro expansion of ~a"
                                         (cons key (cdr x)))) macro-fun
                                         (notify0 "APPLY MACRO: ~a" (cons key (cdr x)))
                                         (let ((macro-expanded-form
                                                (progn (setq *pass* 'execute)
                                                       (apply macro-fun (cdr x)))))
                                           (notify0 "RESULT: ~a" macro-expanded-form)
                                           (e macro-expanded-form env e)))))))))

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
;;; IDENTIFIER expander
;;;-----------------------------------------------------------------------------
(defun get-id-expander (key)
  (lambda (x env e)
    (let ((node (or (get-local-static-binding x env)
                    (get-lexical-binding x)
                    (get-t-node x)
                    (get-keyword-node x)
                    (ct-serious-warning
                     (make-dummy-binding x)
                     "no lexical binding ~a available" x))))
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
    (expand-expr `(lambda ,args (,binding-name ,@args)))))

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
;;; APPLICATION expander
;;  (+ - * / % = < <= > >= can be simplified in some cases into
;;  int-binary or int-inc/inc-dec calls assuming a trap to the gf from the
;;  correspondin vm instruction)
;;;-----------------------------------------------------------------------------
(defun get-appl-expander (key)
  (lambda (x env e)
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
             (e (unfold-rest-arg-appl op params) env e))
            ((and (eq op '>)
                  (= 2 nargs)
                  (eq (binding-module? (get-lexical-binding op)) 'compare))
             (e `(int-binary< ,@(reverse params)) env e))
            ((and (or (eq op '=) (eq op '<))
                  (= 2 nargs)
                  (eq (binding-module? (get-lexical-binding op)) 'compare))
             (e (unfold-rest-arg-appl op params) env e))
            ((and (eq op '>=)
                  (= 2 nargs)
                  (eq (binding-module? (get-lexical-binding op)) 'compare))
             (e `(if (int-binary< ,@(reverse params))
                     t
                   (int-binary= ,@params))
                env e))
            ((and (eq op '<=)
                  (= 2 nargs)
                  (eq (binding-module? (get-lexical-binding op)) 'compare))
             (e `(if (int-binary< ,@params)
                     t
                   (int-binary= ,@params)) env e))
            ((or (and (eq op 'int-binary+)
                      (eq (binding-module? (get-lexical-binding op)) 'boot1))
                 (and (eq op 'binary+)
                      (eq (binding-module? (get-lexical-binding op)) 'number)))
             (let ((arg1 (car params))
                   (arg2 (cadr params)))
               (cond ((and (number? arg1)
                           (number? arg2))
                      ;; partial evaluation
                      (e (+ arg1 arg2) env e))
                     ((and (integer? arg1)
                           (= arg1 1))
                      (e `(inc ,arg2) env e))
                     ((and (integer? arg2)
                           (= arg2 1))
                      (e `(inc ,arg1) env e))
                     ((and (integer? arg2)
                           (= arg2 -1))
                      (e `(dec ,arg1) env e))
                     (t
                      (default-appl-expander op params nargs env)))))
            ((or (and (eq op 'int-binary-)
                      (eq (binding-module? (get-lexical-binding op)) 'boot1))
                 (and (eq op 'binary-)
                      (eq (binding-module? (get-lexical-binding op)) 'number)))
             (let ((arg1 (car params))
                   (arg2 (cadr params)))
               (cond ((and (number? arg1)
                           (number? arg2))
                      ;; partial evaluation
                      (e (- arg1 arg2) env e))
                     ;; x - 1
                     ((and (integer? arg2)
                           (= arg2 1))
                      (e `(dec ,arg1) env e))
                     ;; x - -1
                     ((and (integer? arg2)
                           (= arg2 -1))
                      (e `(inc ,arg1) env e))
                     (t
                      (default-appl-expander op params nargs env)))))
            ((or (and (eq op 'int-binary=)
                      (eq (binding-module? (get-lexical-binding op)) 'boot1))
                 (and (eq op 'binary=)
                      (eq (binding-module? (get-lexical-binding op)) 'compare)))
             (let ((arg1 (car params))
                   (arg2 (cadr params)))
               (cond ((and (number? arg1)
                           (number? arg2))
                      ;; partial evaluation
                      (e (= arg1 arg2) env e))
                     ;; x = 0
                     ((and (integer? arg2)
                           (= arg2 0))
                      (e `(int-zero? ,arg1) env e))
                     ;; 0 = x
                     ((and (integer? arg1)
                           (= arg1 0))
                      (e `(int-zero? ,arg2) env e))
                     (t
                      (default-appl-expander op params nargs env)))))
            ((and (cons? op) (eq (car op) 'lambda))
             (e `((inlined-lambda ,@(cdr op)) ,@params) env e))
            (t
             (default-appl-expander op params nargs env))))))

(defun default-appl-expander (op params nargs env)
  (let* ((fun (expand-fun-form op env))
         (args (expand-exprs params env))
         (lifted-appl (lift-appl (cons fun args) () () env))
         (new-exprs (car lifted-appl))
         (new-fun (car new-exprs))
         (new-args (cdr new-exprs))
         (new-vars (car (cdr lifted-appl)))
         (appl (make <appl> fun: new-fun args: new-args)))
    (check-appl appl new-fun)
    (if (null? new-vars)
        appl
      (progn
        (do1-list (lambda (var)
                    (var-used! var (+ (var-used? var) 1)))
                  new-vars)
        (make-let* new-vars appl)))))

(defun unfold-rest-arg-appl (op args)
  (let ((binary-op (concatenate 'int-binary op)))
    (labels
     ((loop (l)
            (if (null? (cdr l))
                (car l)
              (list binary-op (loop (cdr l)) (car l)))))
     (loop (reverse-list args)))))

(defun expand-fun-form (x env)
  ;; Like id expander, but binding is not checked
  (if (atom? x)
      (let ((binding (or (get-local-static-binding x env)
                         (get-lexical-binding x)
                         (ct-serious-warning
                          (make-dummy-binding x)
                          "no lexical binding ~a available" x))))
        (register-binding-ref binding)
        binding)
    (if (and (eq (car x) 'setter) (symbol? (cadr x))
             (null? *interpreter*))
        (or (get-inlined-setter-binding x env)
            (expand-expr x env))
      (expand-expr x env))))

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
(defgeneric check-appl (appl op))

(defmethod check-appl ((appl <appl>) (op <object>))
  (if (function? op)
      (ct-serious-warning () "macro binding ~a should be in syntax import"
                          (binding-local-name? (appl-fun? appl)))
    (ct-serious-warning () "no applicable object ~a" op)))

(defmethod check-appl ((appl <appl>) (op <fun>))
  (check-appl-arity appl op (local-name? op))
  (fun-appls! op (cons appl (fun-appls? op))))

(defmethod check-appl ((appl <appl>) (op <interface-binding>))
  ;; e.g. could check arity, when written in interface file
  ())

(defmethod check-appl ((appl <appl>) (op <binding>))
  (and (binding-obj? op)              ; no check with dummy binding
       (check-appl appl (binding-obj? op))))

(defmethod check-appl ((appl <appl>) (op <var>))
  ;; is ok
  ())

(defmethod check-appl ((appl <appl>) (op <named-const>))
  ;; is ok
  ())

;;;-----------------------------------------------------------------------------
;;; CALL-NEXT-METHOD expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'call-next-method
                       (lambda (x env e)
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
;;; Install IF expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'if
                       (lambda (x env e)
                         (with-ct-handler (fmt "bad if syntax ~a" x) x
                                          (let* ((pred-expr (cadr x))
                                                 (then-expr (caddr x))
                                                 (else-expr1 (cdddr x))
                                                 (else-expr (and else-expr1 (car else-expr1)))
                                                 (pred-node (expand-expr pred-expr env))
                                                 (then-node (expand-expr then-expr env))
                                                 (else-node (expand-expr else-expr env)))
                                            (if (or *no-else* else-expr1)
                                                (if (cdddr (cdr x))
                                                    (ct-serious-warning () "bad if syntax (if ~a ...)" pred-expr)
                                                  ())
                                              (ct-warning () "missing else branch in (if ~a ...)" pred-expr))
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
                    (if ,var-name ,then-e ,else-e)) env)))
;;;-----------------------------------------------------------------------------
;;; Install PROGN expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'progn
                       (lambda (x env e)
                         (cond ((null? (cdr x))                     ; no empty progn?
                                *nil*)
                               ((null? (cdr (cdr x)))               ; no one-form progn?
                                (expand-expr (car (cdr x)) env))
                               (t
                                (let ((exprs ())
                                      (last-expr ())
                                      (dummy-name (gensym)))
                                  (do1-list-last-special
                                   (lambda (x) (setq exprs (cons (list dummy-name x) exprs)))
                                   (lambda (x) (setq last-expr x))
                                   (cdr x))
                                  (expand-expr `(let* ,(reverse-list exprs) ,last-expr) env))))))

;;;-----------------------------------------------------------------------------
;;; Install RETURN expander
;;;-----------------------------------------------------------------------------
;  (install-expr-expander 'return
;    (lambda (x env e)
;      (make <return> form: (expand-expr (cadr x) env))))

;;;-----------------------------------------------------------------------------
;;; Install QUOTE expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'quote
                       (lambda (x env e)
                         (with-ct-handler "bad quote syntax" x
                                          (make <literal-const> value: (cadr x)))))

;  (defun trailing-colon? (sym)
;    (let ((str (symbol-name sym)))
;      (eq (string-ref str (- (string-size str) 1)) #\:)))

;;;-----------------------------------------------------------------------------
;;; Install QUASIQUOTE expander (sometimes called backquote)
;;;-----------------------------------------------------------------------------
(install-expr-expander 'quasiquote
                       (lambda (xx env e)
                         (with-ct-handler "bad quasiquote syntax" xx
                                          ;; not tail recursive (is it possible?)
                                          (let ((x (car (cdr xx))))
                                            (if (atom? x)
                                                (e (list 'quote x) env e)
                                              (if (eq (car x) 'unquote)
                                                  (e (car (cdr x)) env e)
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
                                                 (e (loop x) env e))))))))

;;;-----------------------------------------------------------------------------
;;; Install SETQ expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'setq
                       (lambda (x env e)
                         (with-ct-handler "bad setq syntax" x
                                          (dynamic-let ((tail-pos? ()))
                                                       (let* ((name (cadr x))
                                                              (binding (or (get-local-static-binding name env)
                                                                           (get-lexical-binding name)
                                                                           (ct-serious-warning
                                                                            (make-dummy-binding name)
                                                                            "no binding ~a available" name)))
                                                              (obj (binding-obj? binding))
                                                              (set-immutable (cdr (cdr (cdr x)))))
                                                         (register-binding-ref binding)
                                                         (and (binding-immutable? binding)
                                                              (null? set-immutable)
                                                              (ct-serious-warning
                                                               () "immutable binding ~a cannot be modified" name))
                                                         (lift-setq binding (expand-expr (caddr x) env) env))))))

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
;;; Install LAMBDA expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'lambda
                       (lambda (x env e)
                         (with-ct-handler "bad lambda syntax" x
                                          (let ((node (make-fun <lambda> 'anonymous
                                                                (get-lambda-params x) (get-lambda-body x)
                                                                t)))      ; has unknown applications
                                            (complete-lambda-node node env)
                                            node))))

(install-expr-expander 'named-lambda
                       (lambda (x env e)
                         (with-ct-handler "bad named lambda syntax" x
                                          (let* ((name (make-symbol (fmt "~a" (car (cdr x)))))
                                                 (node (make-fun <lambda> (list name)
                                                                 (get-params x) (get-body x)
                                                                 t)))     ; has unknown applications
                                            (complete-lambda-node node env)
                                            node))))

(defun complete-lambda-node (fun . envs)
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
                   (fun-body! fun (expand-expr (fun-body? fun) new-env))))))

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
;;; Install INLINED-LAMBDA expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'inlined-lambda
                       (lambda (x env e)
                         (with-ct-handler "bad lambda syntax" x
                                          (let ((node (make-fun <lambda> 'anonymous
                                                                (get-lambda-params x) (get-lambda-body x)
                                                                t)))      ; has unknown applications
                                            (lambda-inlined! node t)
                                            (complete-lambda-node node env)
                                            node))))

;;;-----------------------------------------------------------------------------
;;; Install OPENCODED-LAMBDA expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'opencoded-lambda
                       (lambda (x env e)
                         (with-ct-handler "bad opencoded-lambda syntax" x
                                          (let ((node (make-fun <opencoding> 'anonymous
                                                                (get-lambda-params x) (get-lambda-body x)
                                                                t)))      ; unknown applications
                                            (fun-body! node (cdr (fun-body? node))) ; remove default progn
                                            node))))

;;;-----------------------------------------------------------------------------
;;; Install LET expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'let
                       (lambda (x env e)
                         (with-ct-handler "bad let syntax" x
                                          (let ((decls (car (cdr x)))
                                                (body (cdr (cdr x))))
                                            (cond ((null? decls)                 ; empty let
                                                   (e `(progn ,@body) env e))
                                                  ((cons? decls)                ; simple let
                                                   (if (= (list-size decls) 1)  ; let -> let*
                                                       (e `(let* ,decls ,@body) env e)
                                                     (let* ((args (filter-vars decls))
                                                            (init-forms (filter-init-forms decls)))
                                                       (e `((inlined-lambda ,args ,@body) ,@init-forms) env e))))
                                                  ((symbol? decls)              ; named let
                                                   (let ((named-let-clauses (car body))
                                                         (named-let-body (cdr body)))
                                                     (e `(labels
                                                          ((,decls (,@(map1-list car named-let-clauses))
                                                                   ,@named-let-body))
                                                          (,decls ,@(map1-list (lambda (x) (car (cdr x)))
                                                                               named-let-clauses)))
                                                        env e))))))))

;;;-----------------------------------------------------------------------------
;;; Install LET* expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'let*
                       (lambda (x env e)
                         (with-ct-handler "bad let* syntax" x
                                          (cond ((null? (cadr x))
                                                 (expand-expr (list 'progn (caddr x)) env))
                                                ((cons? (cadr x))
                                                 (let* ((inits (filter-init-forms (cadr x)))
                                                        (vars-and-env (expand-local-static-vars*
                                                                       (filter-vars (cadr x)) inits env))
                                                        (vars (car vars-and-env))
                                                        (new-env (cdr vars-and-env))
                                                        (body (expand-expr `(progn ,@(cdr (cdr x))) new-env))
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
;;; Install LABELS expander
;;;-----------------------------------------------------------------------------
(install-expr-expander 'labels
                       (lambda (x env e)
                         (with-ct-handler "bad labels syntax" x
                                          (let ((decls (car (cdr x)))
                                                (body (cdr (cdr x))))
                                            (expand-expr `(let ,(labelsvar decls)
                                                            ,@(labelssetq decls)
                                                            ,@body) env)))))

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
;;; Expand LOCAL-STATIC-VARS
;;;-----------------------------------------------------------------------------
(defun expand-local-static-vars (var-names init-forms env)
  (let ((true-var-names (as-proper-list var-names)))
    (and (null? init-forms)
         (setq init-forms (map1-list null? true-var-names)))
    (map2-list (lambda (var init-form)
                 (make-local-static-var var (expand-expr init-form env)))
               true-var-names init-forms)))

(defun expand-local-static-vars* (var-names init-forms env)
  (labels
   ((loop (vars forms env)
          (if (null? vars) env
            (let* ((var (car vars))
                   (init-form (car forms))
                   (new-env (add-local-static-bindings
                             (list (var-binding? var)) env)))
              (var-value! var (expand-expr init-form env))
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
)  ;; End of module
;;;-----------------------------------------------------------------------------
