;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; match.em -- modulized version of match-slib.scm badly hacked for eulisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes:
;; * Vectors probably don't work right.
;;   Why doesn't #(a b c ...) match what PLT Scheme gives for the same thing?
;; * Structures certainly don't.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule match
  (syntax ((except (match-let) macros) match0)
   import (level1 match-support)
   export (match match-lambda match-lambda* match-let match-let*))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Pattern Matching Syntactic Extensions for Scheme
  ;;
  (defconstant match:version "Version 1.18, July 17, 1995")
  ;;
  ;; Report bugs to wright@research.nj.nec.com.  The most recent version of
  ;; this software can be obtained by anonymous FTP from ftp.nj.nec.com
  ;; in file pub/wright/match.tar.Z.  Be sure to set "type binary" when
  ;; transferring this file.
  ;;
  ;; Written by Andrew K. Wright, 1993 (wright@research.nj.nec.com).
  ;; Adapted from code originally written by Bruce F. Duba, 1991.
  ;; This package also includes a modified version of Kent Dybvig's
  ;; define-structure (see Dybvig, R.K., The Scheme Programming Language,
  ;; Prentice-Hall, NJ, 1987).
  ;;
  ;; This software is in the public domain.  Feel free to copy,
  ;; distribute, and modify this software as desired.  No warranties
  ;; nor guarantees of any kind apply.  Please return any improvements
  ;; or bug fixes to wright@research.nj.nec.com so that they may be included
  ;; in future releases.
  ;;
  ;; This macro package extends Scheme with several new expression forms.
  ;; Following is a brief summary of the new forms.  See the member-alistiated
  ;; LaTeX documentation for a full description of their functionality.
  ;;
  ;;
  ;;         match expressions:
  ;;
  ;; exp ::= ...
  ;;       | (match exp clause ...)
  ;;       | (match-lambda clause ...)
  ;;       | (match-lambda* clause ...)
  ;;       | (match-let ((pat exp) ...) body)
  ;;       | (match-let* ((pat exp) ...) body)
  ;;       | (match-letrec ((pat exp) ...) body)
  ;;       | (match-define pat exp)
  ;;
  ;; clause ::= (pat body) | (pat => exp)
  ;;
  ;;         patterns:                       matches:
  ;;
  ;; pat ::= identifier                      anything, and binds identifier
  ;;       | _                               anything
  ;;       | ()                              the empty list
  ;;       | t                              t
  ;;       | string                          a string
  ;;       | number                          a number
  ;;       | character                       a character
  ;;       | 'sexp                           an s-expression
  ;;       | 'symbol                         a symbol (special case of s-expr)
  ;;       | (pat_1 ... pat_n)               list of n elements
  ;;       | (pat_1 ... pat_n . pat_{n+1})   list of n or more
  ;;       | (pat_1 ... pat_n pat_n+1 ooo)   list of n or more, each element
  ;;                                           of remainder must match pat_n+1
  ;;       | #(pat_1 ... pat_n)              vector of n elements
  ;;       | #(pat_1 ... pat_n pat_n+1 ooo)  vector of n or more, each element
  ;;                                           of remainder must match pat_n+1
  ;;       | #&pat                           box
  ;;       | ($ struct-name pat_1 ... pat_n) a structure
  ;;       | (= field pat)                   a field of a structure
  ;;       | (and pat_1 ... pat_n)           if all of pat_1 thru pat_n match
  ;;       | (or pat_1 ... pat_n)            if any of pat_1 thru pat_n match
  ;;       | (not pat_1 ... pat_n)           if all pat_1 thru pat_n don't match
  ;;       | (? predicate pat_1 ... pat_n)   if predicate true and all of
  ;;                                           pat_1 thru pat_n match
  ;;       | (setq identifier)               anything, and binds setter
  ;;       | (get! identifier)               anything, and binds getter
  ;;       | `qp                             a quasi-pattern
  ;;
  ;; ooo ::= ...                             zero or more
  ;;       | ___                             zero or more
  ;;       | ..k                             k or more
  ;;       | __k                             k or more
  ;;
  ;;         quasi-patterns:                 matches:
  ;;
  ;; qp  ::= ()                              the empty list
  ;;       | t                              t
  ;;       | ()                              ()
  ;;       | string                          a string
  ;;       | number                          a number
  ;;       | character                       a character
  ;;       | identifier                      a symbol
  ;;       | (qp_1 ... qp_n)                 list of n elements
  ;;       | (qp_1 ... qp_n . qp_{n+1})      list of n or more
  ;;       | (qp_1 ... qp_n qp_n+1 ooo)      list of n or more, each element
  ;;                                           of remainder must match qp_n+1
  ;;       | #(qp_1 ... qp_n)                vector of n elements
  ;;       | #(qp_1 ... qp_n qp_n+1 ooo)     vector of n or more, each element
  ;;                                           of remainder must match qp_n+1
  ;;       | #&qp                            box
  ;;       | ,pat                            a pattern
  ;;       | ,@pat                           a pattern
  ;;
  ;; The names (quote, quasiquote, unquote, unquote-splicing, ?, _, $,
  ;; and, or, not, setq, get!, ..., ___) cannot be used as pattern variables.
  ;;
  ;;
  ;;         structure expressions:
  ;;
  ;; exp ::= ...
  ;;       | (define-structure (id_0 id_1 ... id_n))
  ;;       | (define-structure (id_0 id_1 ... id_n)
  ;;                           ((id_{n+1} exp_1) ... (id_{n+m} exp_m)))
  ;;       | (define-const-structure (id_0 arg_1 ... arg_n))
  ;;       | (define-const-structure (id_0 arg_1 ... arg_n)
  ;;                                 ((arg_{n+1} exp_1) ... (arg_{n+m} exp_m)))
  ;;
  ;; arg ::= id | (! id) | (@ id)
  ;;
  ;;
  ;; match:error-control controls what code is generated for failed matches.
  ;; Possible values:
  ;;  'unspecified - do nothing, ie., evaluate (cond [() ()])
  ;;  'fail - call match:error, or die at car or cdr
  ;;  'error - call match:error with the unmatched value
  ;;  'match - call match:error with the unmatched value _and_
  ;;             the quoted match expression
  ;; match:error-control is set by calling match:set-error-control with
  ;; the new value.
  ;;
  ;; match:error is called for a failed match.
  ;; match:error is set by calling match:set-error with the new value.
  ;;
  ;; match:structure-control controls the uniqueness of structures
  ;; (does not exist for Scheme 48 version).
  ;; Possible values:
  ;;  'vector - (default) structures are vectors with a symbol in position 0
  ;;  'disjoint - structures are fully disjoint from all other values
  ;; match:structure-control is set by calling match:set-structure-control
  ;; with the new value.
  ;;
  ;; match:runtime-structures controls whether local structure declarations
  ;; generate new structures each time they are reached
  ;; (does not exist for Scheme 48 version).
  ;; Possible values:
  ;;  t - (default) each runtime occurrence generates a new structure
  ;;  () - each lexical occurrence generates a new structure
  ;;
  ;; End of user visible/modifiable stuff.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; These expander functions are not user visible.
  (defconstant match:expanders
    (letrec
      ((genmatch
         (lambda (x clauses match-expr)
           (let* ((size>= (gensym))
                  (eb-errf (error-maker match-expr))
                  (blist (car eb-errf))
                  (plist (map
                           (lambda (c)
                             (let* ((x (bound (validate-pattern (car c))))
                                    (p (car x))
                                    (bv (cadr x))
                                    (bindings (caddr x))
                                    (code (gensym))
                                    (fail (and (cons? (cdr c))
                                               (cons? (cadr c))
                                               (eq (caadr c) '=>)
                                               (symbol? (cadadr c))
                                               (cons? (cdadr c))
                                               (null? (cddadr c))
                                               (cons? (cddr c))
                                               (cadadr c)))
                                    (bv2 (if fail
                                             (cons fail bv)
                                           bv))
                                    (body (if fail
                                              (cddr c)
                                            (cdr c))))
                               (setq blist
                                     (cons `(,code
                                              (lambda ,bv2
                                                ,@body))
                                           (append bindings blist)))
                               (list p code bv (and fail (gensym)) ())))
                           clauses))
                  (code (gen x '() plist (cdr eb-errf) size>= (gensym))))
             (unreachable plist match-expr)
             (inline-let
               `(let ((,size>= (lambda (n)
                                   (lambda (l)
                                     (>= (size l) n))))
                      ,@blist)
                  ,code)))))
       (genletrec
         (lambda (pat exp body match-expr)
           (let* ((size>= (gensym))
                  (eb-errf (error-maker match-expr))
                  (x (bound (validate-pattern pat)))
                  (p (car x))
                  (bv (cadr x))
                  (bindings (caddr x))
                  (code (gensym))
                  (plist (list (list p code bv () ())))
                  (x (gensym))
                  (m (gen x '() plist (cdr eb-errf) size>= (gensym)))
                  (gs (map (lambda (_) (gensym)) bv)))
             (unreachable plist match-expr)
             `(letrec ((,size>= (lambda (n)
                                    (lambda (l)
                                      (>= (size l) n))))
                       ,@(map (lambda (v) `(,v ())) bv)
                       (,x ,exp)
                       (,code (lambda ,gs
                                ,@(map (lambda (v g) `(setq ,v ,g)) bv gs)
                                ,@body))
                       ,@bindings
                       ,@(car eb-errf))
                      ,m))))
       (gendefine
         (lambda (pat exp match-expr)
           (let* ((size>= (gensym))
                  (eb-errf (error-maker match-expr))
                  (x (bound (validate-pattern pat)))
                  (p (car x))
                  (bv (cadr x))
                  (bindings (caddr x))
                  (code (gensym))
                  (plist (list (list p code bv () ())))
                  (x (gensym))
                  (m (gen x '() plist (cdr eb-errf) size>= (gensym)))
                  (gs (map (lambda (_) (gensym)) bv)))
             (unreachable plist match-expr)
             `(progn ,@(map (lambda (v) `(deflocal ,v ())) bv)
                     ,(inline-let
                        `(let ((,size>=
                                 (lambda (n) (lambda (l) (>= (size l) n))))
                               (,x ,exp)
                               (,code
                                 (lambda ,gs
                                   ,@(map (lambda (v g) `(setq ,v ,g)) bv gs)
                                   (cond (() ()))))
                               ,@bindings
                               ,@(car eb-errf))
                           ,m))))))
       (pattern-var?
         (lambda (x)
           (and (symbol? x)
                (not (dot-dot-k? x))
                (not (memq x '(quasiquote quote unquote unquote-splicing
                                          ? _ $ = and or not setq get!
                                          ... ___))))))
       (dot-dot-k?
         (lambda (s)
           (and (symbol? s)
                (if (memq s '(... ___))
                    0
                  (let* ((s (convert s <string>))
                         (n (string-size s)))
                    (and (<= 3 n)
                         (memq (string-ref s 0) '(#\. #\_))
                         (memq (string-ref s 1) '(#\. #\_))
                         (match:andmap digit?
                                       (convert (substring s 2 n) <list>))
                         (convert (substring s 2 n) <int>)))))))
       (error-maker
         (lambda (match-expr)
           (cond
             ((eq match:error-control 'unspecified)
              (cons '() (lambda (x) `(cond (() ())))))
             ((memq match:error-control '(error fail))
              (cons '() (lambda (x) `(match:error ,x))))
             ((eq match:error-control 'match)
              (let ((errf (gensym))
                    (arg (gensym)))
                (cons `((,errf (lambda (,arg)
                                 (match:error ,arg ',match-expr))))
                      (lambda (x) `(,errf ,x)))))
             (else
               (match:syntax-err
                 '(unspecified error fail match)
                 "invalid value for match:error-control, legal values are")))))
       (unreachable
         (lambda (plist match-expr)
           (do
             (lambda (x)
               (let ((exp (car (cddddr x))))
                 (if (not (car (cddddr x)))
                     (progn (prin
                              "Warning: unreachable pattern ")
                            (prin (car x))
                            (prin " in ")
                            (prin match-expr)
                            (newline))
                   ())))
             plist)))
       (validate-pattern
         (lambda (pattern)
           (letrec
             ((simple?
                (lambda (x)
                  (or (string? x)
                      (boolean? x)
                      (character? x)
                      (number? x)
                      (null? x))))
              (ordinary
                (lambda (p)
                  (let ((g88 (lambda (x y)
                               (cons (ordinary x) (ordinary y)))))
                    (if (simple? p)
                        ((lambda (p) p) p)
                      (if (binary= p '_)
                          ((lambda () '_))
                        (if (pattern-var? p)
                            ((lambda (p) p) p)
                          (if (cons? p)
                              (if (binary= (car p) 'quasiquote)
                                  (if (and (cons? (cdr p))
                                           (null? (cddr p)))
                                      ((lambda (p) (quasi p))
                                       (cadr p))
                                    (g88 (car p) (cdr p)))
                                (if (binary= (car p) 'quote)
                                    (if (and (cons? (cdr p))
                                             (null? (cddr p)))
                                        ((lambda (p) p) p)
                                      (g88 (car p) (cdr p)))
                                  (if (binary= (car p) '?)
                                      (if (and (cons? (cdr p))
                                               (list? (cddr p)))
                                          ((lambda (pred ps)
                                             `(? ,pred ,@(map ordinary ps)))
                                           (cadr p)
                                           (cddr p))
                                        (g88 (car p)
                                             (cdr p)))
                                    (if (binary= (car p) '=)
                                        (if (and (cons? (cdr p))
                                                 (cons? (cddr p))
                                                 (null? (cdddr p)))
                                            ((lambda (sel p)
                                               `(= ,sel ,(ordinary p)))
                                             (cadr p)
                                             (caddr p))
                                          (g88 (car p) (cdr p)))
                                      (if (binary= (car p) 'and)
                                          (if (and (list? (cdr p))
                                                   (cons? (cdr p)))
                                              ((lambda (ps)
                                                 `(and ,@(map ordinary ps)))
                                               (cdr p))
                                            (g88 (car p) (cdr p)))
                                        (if (binary= (car p) 'or)
                                            (if (and (list? (cdr p))
                                                     (cons? (cdr p)))
                                                ((lambda (ps)
                                                   `(or ,@(map ordinary ps)))
                                                 (cdr p))
                                              (g88 (car p) (cdr p)))
                                          (if (binary= (car p) 'not)
                                              (if (and (list? (cdr p))
                                                       (cons? (cdr p)))
                                                  ((lambda (ps)
                                                     `(not ,@(map ordinary ps)))
                                                   (cdr p))
                                                (g88 (car p) (cdr p)))
                                            (if (binary= (car p) '$)
                                                (if (and (cons? (cdr p))
                                                         (symbol? (cadr p))
                                                         (list? (cddr p)))
                                                    ((lambda (r ps)
                                                       `($ ,r ,@(map ordinary ps)))
                                                     (cadr p)
                                                     (cddr p))
                                                  (g88 (car p) (cdr p)))
                                              (if (binary= (car p) 'setq)
                                                  (if (and (cons? (cdr p))
                                                           (pattern-var? (cadr p))
                                                           (null? (cddr p)))
                                                      ((lambda (p) p) p)
                                                    (g88 (car p) (cdr p)))
                                                (if (binary= (car p) 'get!)
                                                    (if (and (cons? (cdr p))
                                                             (pattern-var? (cadr p))
                                                             (null? (cddr p)))
                                                        ((lambda (p) p) p)
                                                      (g88 (car p) (cdr p)))
                                                  (if (binary= (car p) 'unquote)
                                                      (g88 (car p) (cdr p))
                                                    (if (binary= (car p)
                                                                'unquote-splicing)
                                                        (g88 (car p) (cdr p))
                                                      (if (and (cons? (cdr p))
                                                               (dot-dot-k? (cadr p))
                                                               (null? (cddr p)))
                                                          ((lambda (p ddk)
                                                             `(,(ordinary p) ,ddk))
                                                           (car p)
                                                           (cadr p))
                                                        (g88 (car p)
                                                             (cdr p)))))))))))))))
                            (if (vector? p)
                                ((lambda (p)
                                   (let* ((pl (convert p <list>))
                                          (rpl (reverse pl)))
                                     (apply
                                       (lambda l (convert l <vector>))
                                       (if (and (not (null? rpl))
                                                (dot-dot-k? (car rpl)))
                                           (reverse
                                             (cons (car rpl)
                                                   (map ordinary (cdr rpl))))
                                         (map ordinary pl)))))
                                 p)
                              ((lambda ()
                                 (match:syntax-err
                                   pattern
                                   "syntax error in pattern")))))))))))
              (quasi
                (lambda (p)
                  (let ((g109 (lambda (x y)
                                (cons (quasi x) (quasi y)))))
                    (if (simple? p)
                        ((lambda (p) p) p)
                      (if (symbol? p)
                          ((lambda (p) `',p) p)
                        (if (cons? p)
                            (if (binary= (car p) 'unquote)
                                (if (and (cons? (cdr p))
                                         (null? (cddr p)))
                                    ((lambda (p) (ordinary p))
                                     (cadr p))
                                  (g109 (car p) (cdr p)))
                              (if (and (cons? (car p))
                                       (binary= (caar p)
                                               'unquote-splicing)
                                       (cons? (cdar p))
                                       (null? (cddar p)))
                                  (if (null? (cdr p))
                                      ((lambda (p) (ordinary p))
                                       (cadar p))
                                    ((lambda (p y)
                                       (append (ordlist p)
                                               (quasi y)))
                                     (cadar
                                       p)
                                     (cdr p)))
                                (if (and (cons? (cdr p))
                                         (dot-dot-k? (cadr p))
                                         (null? (cddr p)))
                                    ((lambda (p ddk)
                                       `(,(quasi p) ,ddk))
                                     (car p) (cadr p))
                                  (g109 (car p) (cdr p)))))
                          (if (vector? p)
                              ((lambda (p)
                                 (let* ((pl (convert p <list>))
                                        (rpl (reverse pl)))
                                   (apply
                                     (lambda l (convert l <vector>))
                                     (if (dot-dot-k? (car rpl))
                                         (reverse
                                           (cons (car rpl)
                                                 (map quasi
                                                      (cdr rpl))))
                                       (map ordinary pl)))))
                               p)
                            ((lambda ()
                               (match:syntax-err
                                 pattern
                                 "syntax error in pattern"))))))))))
              (ordlist
                (lambda (p)
                  (cond
                    ((null? p) '())
                    ((cons? p) (cons (ordinary (car p))
                                     (ordlist (cdr p))))
                    (else
                      (match:syntax-err
                        pattern
                        "invalid use of unquote-splicing in pattern"))))))
             (ordinary pattern))))
       (bound
         (lambda (pattern)
           (letrec
             ((pred-bodies '())
              (bound
                (lambda (p a k)
                  (cond
                    ((eq '_ p)
                     (k p a))
                    ((symbol? p)
                     (if (memq p a)
                         (match:syntax-err
                           pattern
                           "duplicate variable in pattern")
                       ())
                     (k p (cons p a)))
                    ((and (cons? p)
                          (eq 'quote (car p)))
                     (k p a))
                    ((and (cons? p) (eq '? (car p)))
                     (cond
                       ((not (null? (cddr p)))
                        (bound `(and (? ,(cadr p)) ,@(cddr p)) a k))
                       ((or (not (symbol? (cadr p)))
                            (memq (cadr p) a))
                        (let ((g (gensym)))
                          (setq pred-bodies
                                (cons `(,g ,(cadr p)) pred-bodies))
                          (k `(? ,g) a)))
                       (else
                         (k p a))))
                    ((and (cons? p)
                          (eq '= (car p)))
                     (cond
                       ((or (not (symbol? (cadr p)))
                            (memq (cadr p) a))
                        (let ((g (gensym)))
                          (setq pred-bodies
                                (cons `(,g ,(cadr p)) pred-bodies))
                          (bound `(= ,g ,(caddr p)) a k)))
                       (else
                         (bound (caddr p)
                                a
                                (lambda (p2 a)
                                  (k `(= ,(cadr p) ,p2) a))))))
                    ((and (cons? p)
                          (eq 'and (car p)))
                     (bound* (cdr p)
                             a
                             (lambda (p a) (k `(and ,@p) a))))
                    ((and (cons? p)
                          (eq 'or (car p)))
                     (bound
                       (cadr p)
                       a
                       (lambda (first-p first-a)
                         (let or* ((plist (cddr p))
                                   (k (lambda (plist)
                                        (k `(or ,first-p ,@plist)
                                           first-a))))
                              (if (null? plist)
                                  (k plist)
                                (bound
                                  (car plist)
                                  a
                                  (lambda (car-p car-a)
                                    (if (not (permutation car-a first-a))
                                        (match:syntax-err
                                          pattern
                                          "variables of or-pattern differ in")
                                      ())
                                    (or* (cdr plist)
                                         (lambda (cdr-p)
                                           (k (cons car-p cdr-p)))))))))))
                    ((and (cons? p)
                          (eq 'not (car p)))
                     (cond
                       ((not (null? (cddr p)))
                        (bound `(not (or ,@(cdr p))) a k))
                       (else
                         (bound (cadr p)
                                a
                                (lambda (p2 a2)
                                  (if (not (permutation a a2))
                                      (match:syntax-err
                                        p
                                        "no variables allowed in")
                                    ())
                                  (k `(not ,p2) a))))))
                    ((and (cons? p)
                          (cons? (cdr p))
                          (dot-dot-k? (cadr p)))
                     (bound (car p)
                            a
                            (lambda (q b)
                              (let ((bvars (find-prefix b a)))
                                (k `(,q ,(cadr p)
                                        ,bvars
                                        ,(gensym)
                                        ,(gensym)
                                        ,(map (lambda (_) (gensym))
                                              bvars))
                                   b)))))
                    ((and (cons? p)
                          (eq '$ (car p)))
                     (bound* (cddr p)
                             a
                             (lambda (p1 a) (k `($ ,(cadr p) ,@p1) a))))
                    ((and (cons? p)
                          (eq 'setq (car p)))
                     (if (memq (cadr p) a)
                         (k p a)
                       (k p (cons (cadr p) a))))
                    ((and (cons? p)
                          (eq 'get! (car p)))
                     (if (memq (cadr p) a)
                         (k p a)
                       (k p (cons (cadr p) a))))
                    ((cons? p)
                     (bound
                       (car p)
                       a
                       (lambda (car-p a)
                         (bound
                           (cdr p)
                           a
                           (lambda (cdr-p a)
                             (k (cons car-p cdr-p) a))))))
                    ((vector? p)
                     (boundv (convert p <list>)
                             a
                             (lambda (pl a) (k (convert pl <vector>) a))))
                    (else
                      (k p a)))))
              (boundv
                (lambda (plist a k)
                  (let ((g115 (lambda () (k plist a))))
                    (if (cons? plist)
                        (if (and (cons? (cdr plist))
                                 (dot-dot-k? (cadr plist))
                                 (null? (cddr plist)))
                            ((lambda () (bound plist a k)))
                          (if (null? plist)
                              (g115)
                            ((lambda (x y)
                               (bound
                                 x
                                 a
                                 (lambda (car-p a)
                                   (boundv
                                     y
                                     a
                                     (lambda (cdr-p a)
                                       (k (cons car-p cdr-p) a))))))
                             (car plist)
                             (cdr plist))))
                      (if (null? plist)
                          (g115)
                        (match:error plist))))))
              (bound*
                (lambda (plist a k)
                  (if (null? plist)
                      (k plist a)
                    (bound
                      (car plist)
                      a
                      (lambda (car-p a)
                        (bound*
                          (cdr plist)
                          a
                          (lambda (cdr-p a)
                            (k (cons car-p cdr-p) a))))))))
              (find-prefix
                (lambda (b a)
                  (if (eq b a)
                      '()
                    (cons (car b) (find-prefix (cdr b) a)))))
              (permutation
                (lambda (p1 p2)
                  (and (= (size p1)
                          (size p2))
                       (match:andmap
                         (lambda (x1) (memq x1 p2))
                         p1)))))
             (bound
               pattern
               '()
               (lambda (p a) (list p (reverse a) pred-bodies))))))
       (inline-let
         (lambda (let-exp)
           (letrec ((occ
                      (lambda (x e)
                        (let loop ((e e))
                             (cond
                               ((cons? e) (+ (loop (car e)) (loop (cdr e))))
                               ((eq x e) 1)
                               (else 0)))))
                    (subst
                      (lambda (e old new)
                        (let loop ((e e))
                             (cond
                               ((cons? e) (cons (loop (car e))
                                                (loop (cdr e))))
                               ((eq old e) new)
                               (else e)))))
                    (const?
                      (lambda (sexp)
                        (or (symbol? sexp)
                            (boolean? sexp)
                            (string? sexp)
                            (character? sexp)
                            (number? sexp)
                            (null? sexp)
                            (and (cons? sexp)
                                 (eq (car sexp) 'quote)
                                 (cons? (cdr sexp))
                                 (symbol? (cadr sexp))
                                 (null? (cddr sexp))))))
                    (isval?
                      (lambda (sexp)
                        (or (const? sexp)
                            (and (cons? sexp)
                                 (memq (car sexp) '(lambda quote
                                                     match-lambda
                                                     match-lambda*))))))
                    (small?
                      (lambda (sexp)
                        (or (const? sexp)
                            (and (cons? sexp)
                                 (eq (car sexp) 'lambda)
                                 (cons? (cdr sexp))
                                 (cons? (cddr sexp))
                                 (const? (caddr sexp))
                                 (null? (cdddr sexp)))))))
                   (let loop ((b (cadr let-exp))
                              (new-b '())
                              (e (caddr let-exp)))
                        (cond
                          ((null? b)
                           (if (null? new-b)
                               e
                             `(let ,(reverse new-b) ,e)))
                          ((isval? (cadr (car b)))
                           (let* ((x (caar b))
                                  (n (occ x e)))
                             (cond
                               ((= 0 n) (loop (cdr b) new-b e))
                               ((or (= 1 n)
                                    (small? (cadr (car b))))
                                (loop (cdr b) new-b (subst e x (cadr (car b)))))
                               (else
                                 (loop (cdr b) (cons (car b) new-b) e)))))
                          (else
                            (loop (cdr b) (cons (car b) new-b) e)))))))
       (gen
         (lambda (x sf plist erract size>= eta)
           (if (null? plist)
               (erract x)
             (let* ((v '())
                    (val (lambda (x) (cdr (member-alist x v))))
                    (fail (lambda (sf)
                            (gen x sf (cdr plist) erract size>= eta)))
                    (success (lambda (sf)
                               ((setter car) (cddddr (car plist)) t)
                               (let* ((code (cadr (car plist)))
                                      (bv (caddr (car plist)))
                                      (fail-sym (cadddr (car plist))))
                                 (if fail-sym
                                     (let ((ap `(,code
                                                  ,fail-sym
                                                  ,@(map val bv))))
                                       `(call-with-current-continuation
                                          (lambda (,fail-sym)
                                            (let ((,fail-sym
                                                    (lambda ()
                                                      (,fail-sym
                                                        ,(fail sf)))))
                                              ,ap))))
                                   `(,code ,@(map val bv)))))))
               (let next ((p (caar plist))
                          (e x)
                          (sf sf)
                          (kf fail)
                          (ks success))
                    (cond
                      ((eq '_ p)
                       (ks sf))
                      ((symbol? p)
                       (setq v (cons (cons p e) v))
                       (ks sf))
                      ((null? p)
                       (emit `(null? ,e) sf kf ks))
                      ((binary= p ''())
                       (emit `(null? ,e) sf kf ks))
                      ((string? p)
                       (emit `(binary= ,e ,p) sf kf ks))
                      ((boolean? p)
                       (emit `(binary= ,e ,p) sf kf ks))
                      ((character? p)
                       (emit `(binary= ,e ,p) sf kf ks))
                      ((number? p)
                       (emit `(binary= ,e ,p) sf kf ks))
                      ((and (cons? p)
                            (eq 'quote (car p)))
                       (emit `(binary= ,e ,p) sf kf ks))
                      ((and (cons? p)
                            (eq '? (car p)))
                       (let ((tst `(,(cadr p) ,e)))
                         (emit tst sf kf ks)))
                      ((and (cons? p)
                            (eq '= (car p)))
                       (next (caddr p) `(,(cadr p) ,e) sf kf ks))
                      ((and (cons? p)
                            (eq 'and (car p)))
                       (let loop ((p (cdr p))
                                  (sf sf))
                            (if (null? p)
                                (ks sf)
                              (next (car p)
                                    e
                                    sf
                                    kf
                                    (lambda (sf) (loop (cdr p) sf))))))
                      ((and (cons? p)
                            (eq 'or (car p)))
                       (let ((or-v v))
                         (let loop ((p (cdr p))
                                    (sf sf))
                              (if (null? p)
                                  (kf sf)
                                (progn (setq v or-v)
                                       (next (car p)
                                             e
                                             sf
                                             (lambda (sf) (loop (cdr p) sf))
                                             ks))))))
                      ((and (cons? p)
                            (eq 'not (car p)))
                       (next (cadr p) e sf ks kf))
                      ((and (cons? p)
                            (eq '$ (car p)))
                       (let* ((tag (cadr p))
                              (fields (cdr p))
                              (rlen (size
                                      fields))
                              (tst `(,(symbol-append
                                        tag
                                        '?)
                                     ,e)))
                         (emit tst
                               sf
                               kf
                               (let rloop ((n 1))
                                    (lambda (sf)
                                      (if (= n rlen)
                                          (ks sf)
                                        (next (list-ref
                                                fields
                                                n)
                                              `(,(symbol-append tag '- n) ,e)
                                              sf
                                              kf
                                              (rloop (+ 1 n)))))))))
                      ((and (cons? p)
                            (eq 'setq (car p)))
                       (setq v (cons (cons (cadr p) (setter e p)) v))
                       (ks sf))
                      ((and (cons? p)
                            (eq 'get! (car p)))
                       (setq v (cons (cons (cadr p) (getter e p)) v))
                       (ks sf))
                      ((and (cons? p)
                            (cons? (cdr p))
                            (dot-dot-k? (cadr p)))
                       (emit `(list? ,e)
                             sf
                             kf
                             (lambda (sf)
                               (let* ((k (dot-dot-k? (cadr p)))
                                      (ks
                                        (lambda (sf)
                                          (let ((bound (list-ref p 2)))
                                            (cond
                                              ((eq (car p) '_)
                                               (ks sf))
                                              ((null? bound)
                                               (let* ((ptst
                                                        (next (car p)
                                                              eta
                                                              sf
                                                              (lambda (sf) ())
                                                              (lambda (sf) t)))
                                                      (tst
                                                        (if (and
                                                              (cons? ptst)
                                                              (symbol? (car ptst))
                                                              (cons? (cdr ptst))
                                                              (eq eta (cadr ptst))
                                                              (null? (cddr ptst)))
                                                            (car ptst)
                                                          `(lambda (,eta)
                                                             ,ptst))))
                                                 (assm `(match:andmap ,tst ,e)
                                                       (kf sf)
                                                       (ks sf))))
                                              ((and (symbol? (car p))
                                                    (binary= (list (car p))
                                                            bound))
                                               (next (car p) e sf kf ks))
                                              (else
                                                (let* ((gloop (list-ref p 3))
                                                       (ge (list-ref p 4))
                                                       (fresh (list-ref p 5))
                                                       (p1
                                                         (next
                                                           (car p)
                                                           `(car ,ge)
                                                           sf
                                                           kf
                                                           (lambda (sf)
                                                             `(,gloop
                                                                (cdr ,ge)
                                                                ,@(map
                                                                    (lambda (b f)
                                                                      `(cons ,(val b)
                                                                             ,f))
                                                                    bound
                                                                    fresh))))))
                                                  (setq v
                                                        (append
                                                          (map cons
                                                               bound
                                                               (map
                                                                 (lambda (x)
                                                                   `(reverse ,x))
                                                                 fresh))
                                                          v))
                                                  `(let ,gloop
                                                     ((,ge ,e)
                                                      ,@(map
                                                          (lambda (x) `(,x '()))
                                                          fresh))
                                                     (if (null? ,ge)
                                                         ,(ks sf)
                                                       ,p1)))))))))
                                 (case k
                                       (0 (ks sf))
                                       (1 (emit `(cons? ,e) sf kf ks))
                                       (else
                                         (emit `((,size>= ,k) ,e) sf kf ks)))))))
                      ((cons? p)
                       (emit `(cons? ,e)
                             sf
                             kf
                             (lambda (sf)
                               (next (car p)
                                     (add-a e)
                                     sf
                                     kf
                                     (lambda (sf)
                                       (next (cdr p) (add-d e) sf kf ks))))))
                      ((and (vector? p)
                            (>= (vector-size p) 6)
                            (dot-dot-k? (element p (- (vector-size p)
                                                         5))))
                       (let* ((vlen (- (vector-size p) 6))
                              (k (dot-dot-k? (element p (+ vlen 1))))
                              (minlen (+ vlen k))
                              (bound (element p (+ vlen 2))))
                         (emit `(vector? ,e)
                               sf
                               kf
                               (lambda (sf)
                                 (assm
                                   `(>= (vector-size ,e) ,minlen)
                                   (kf sf)
                                   ((let vloop ((n 0))
                                         (lambda (sf)
                                           (cond
                                             ((not (= n vlen))
                                              (next (element p n)
                                                    `(element ,e ,n)
                                                    sf
                                                    kf
                                                    (vloop (+ 1 n))))
                                             ((eq (element p vlen) '_)
                                              (ks sf))
                                             (else
                                               (let* ((gloop (element p (+ vlen 3)))
                                                      (ind (element p (+ vlen 4)))
                                                      (fresh (element p (+ vlen 5)))
                                                      (p1 (next
                                                            (element p vlen)
                                                            `(element ,e ,ind)
                                                            sf
                                                            kf
                                                            (lambda (sf)
                                                              `(,gloop
                                                                 (- ,ind 1)
                                                                 ,@(map
                                                                     (lambda (b f)
                                                                       `(cons ,(val b)
                                                                              ,f))
                                                                     bound
                                                                     fresh))))))
                                                 (setq v (append (map cons
                                                                      bound fresh) v))
                                                 `(let ,gloop
                                                    ((,ind (- (vector-size ,e) 1))
                                                     ,@(map (lambda (x) `(,x '()))
                                                            fresh))
                                                    (if (> ,minlen ,ind)
                                                        ,(ks sf)
                                                      ,p1)))))))
                                    sf))))))
                      ((vector? p)
                       (let ((vlen (vector-size p)))
                         (emit `(vector? ,e)
                               sf
                               kf
                               (lambda (sf)
                                 (emit `(binary= (vector-size ,e) ,vlen)
                                       sf
                                       kf
                                       (let vloop ((n 0))
                                            (lambda (sf)
                                              (if (= n vlen)
                                                  (ks sf)
                                                (next (element p n)
                                                      `(element ,e ,n)
                                                      sf
                                                      kf
                                                      (vloop (+ 1 n)))))))))))
                      (else
                        (prin
                          "FATAL ERROR IN PATTERN MATCHER")
                        (newline)
                        (error () "THIS NEVER HAPPENS"))))))))
       (emit
         (lambda (tst sf kf ks)
           (cond
             ((in tst sf)
              (ks sf))
             ((in `(not ,tst) sf)
              (kf sf))
             (else
               (let* ((e (cadr tst))
                      (implied (cond
                                 ((eq (car tst) 'binary=)
                                  (let ((p (caddr tst)))
                                    (cond
                                      ((string? p)
                                       `((string? ,e)))
                                      ((boolean? p)
                                       `((boolean? ,e)))
                                      ((character? p)
                                       `((character? ,e)))
                                      ((number? p)
                                       `((number? ,e)))
                                      ((and (cons? p)
                                            (eq 'quote (car p)))
                                       `((symbol? ,e)))
                                      (else '()))))
                                 ((eq (car tst) 'null?)
                                  `((list? ,e)))
                                 ((vec-structure? tst) `((vector? ,e)))
                                 (else '())))
                      (not-imp (case (car tst)
                                     ((list?) `((not (null? ,e))))
                                     (else '())))
                      (s (ks (cons tst (append implied sf))))
                      (k (kf (cons `(not ,tst)
                                   (append not-imp sf)))))
                 (assm tst k s))))))
       (assm
         (lambda (tst f s)
           (cond
             ((binary= s f) s)
             ((and (eq s t) (eq f ()))
              tst)
             ((and (eq (car tst) 'cons?)
                   (memq match:error-control '(unspecified fail))
                   (memq (car f) '(cond match:error))
                   (guarantees s (cadr tst)))
              s)
             ((and (cons? s)
                   (eq (car s) 'if)
                   (binary= (cadddr s) f))
              (if (eq (car (cadr s)) 'and)
                  `(if (and ,tst ,@(cdr (cadr s)))
                       ,(caddr s)
                     ,f)
                `(if (and ,tst ,(cadr s))
                     ,(caddr s)
                   ,f)))
             ((and (cons? s)
                   (binary= (car s) 'call-with-current-continuation)
                   (cons? (cdr s))
                   (cons? (cadr s))
                   (binary= (caadr s) 'lambda)
                   (cons? (cdadr s))
                   (cons? (cadadr s))
                   (null? (cdr (cadadr s)))
                   (cons? (cddadr s))
                   (cons? (car (cddadr s)))
                   (binary= (caar (cddadr s)) 'let)
                   (cons? (cdar (cddadr s)))
                   (cons? (cadar (cddadr s)))
                   (cons? (caadar (cddadr s)))
                   (cons? (cdr (caadar (cddadr s))))
                   (cons? (cadr (caadar (cddadr s))))
                   (binary= (caadr (caadar (cddadr s))) 'lambda)
                   (cons? (cdadr (caadar (cddadr s))))
                   (null? (cadadr (caadar (cddadr s))))
                   (cons? (cddadr (caadar (cddadr s))))
                   (cons? (car (cddadr (caadar (cddadr s)))))
                   (cons? (cdar (cddadr (caadar (cddadr s)))))
                   (null? (cddar (cddadr (caadar (cddadr s)))))
                   (null? (cdr (cddadr (caadar (cddadr s)))))
                   (null? (cddr (caadar (cddadr s))))
                   (null? (cdadar (cddadr s)))
                   (cons? (cddar (cddadr s)))
                   (null? (cdddar (cddadr s)))
                   (null? (cdr (cddadr s)))
                   (null? (cddr s))
                   (binary= f (cadar (cddadr (caadar (cddadr s))))))
              (let ((k (car (cadadr s)))
                    (fail (car (caadar (cddadr s))))
                    (s2 (caddar (cddadr s))))
                `(call-with-current-continuation
                   (lambda (,k)
                     (let ((,fail (lambda () (,k ,f))))
                       ,(assm tst `(,fail) s2))))))
             ((and ()
                   (cons? s)
                   (binary= (car s) 'let)
                   (cons? (cdr s))
                   (cons? (cadr s))
                   (cons? (caadr s))
                   (cons? (cdaadr s))
                   (cons? (car (cdaadr s)))
                   (binary= (caar (cdaadr s)) 'lambda)
                   (cons? (cdar (cdaadr s)))
                   (null? (cadar (cdaadr s)))
                   (cons? (cddar (cdaadr s)))
                   (null? (cdddar (cdaadr s)))
                   (null? (cdr (cdaadr s)))
                   (null? (cdadr s))
                   (cons? (cddr s))
                   (null? (cdddr s))
                   (binary= (caddar (cdaadr s)) f))
              (let ((fail (caaadr s))
                    (s2 (caddr s)))
                `(let ((,fail (lambda () ,f)))
                   ,(assm tst `(,fail) s2))))
             (else `(if ,tst ,s ,f)))))
       (guarantees
         (lambda (code x)
           (let ((a (add-a x)) (d (add-d x)))
             (let loop ((code code))
                  (cond
                    ((not (cons? code)) ())
                    ((memq (car code) '(cond match:error)) t)
                    ((or (binary= code a) (binary= code d)) t)
                    ((eq (car code) 'if) (or (loop (cadr code))
                                              (and (loop (caddr code))
                                                   (loop (cadddr code)))))
                    ((eq (car code) 'lambda) ())
                    ((and (eq (car code) 'let)
                          (symbol? (cadr code))) ())
                    (else (or (loop (car code))
                              (loop (cdr code)))))))))
       (in
         (lambda (e l)
           (or (member e l)
               (and (eq (car e) 'list?)
                    (or (member `(null? ,(cadr e)) l)
                        (member `(cons? ,(cadr e)) l)))
               (and (eq (car e) 'not)
                    (let* ((srch (cadr e))
                           (const-class (equal-test? srch)))
                      (cond
                        (const-class
                          (let mem ((l l))
                               (if (null? l)
                                   ()
                                 (let ((x (car l)))
                                   (or (and (binary=
                                              (cadr x)
                                              (cadr srch))
                                            (disjoint? x)
                                            (not (binary=
                                                   const-class
                                                   (car x))))
                                       (binary= x `(not (,const-class
                                                         ,(cadr srch))))
                                       (and (binary= (cadr x) (cadr srch))
                                            (equal-test? x)
                                            (not (binary= (caddr srch)
                                                         (caddr x))))
                                       (mem (cdr l)))))))
                        ((disjoint? srch)
                         (let mem ((l l))
                              (if (null? l)
                                  ()
                                (let ((x (car l)))
                                  (or (and (binary= (cadr x) (cadr srch))
                                           (disjoint? x)
                                           (not (binary= (car x) (car srch))))
                                      (mem (cdr l)))))))
                        ((eq (car srch) 'list?)
                         (let mem ((l l))
                              (if (null? l)
                                  ()
                                (let ((x (car l)))
                                  (or (and (binary= (cadr x) (cadr srch))
                                           (disjoint? x)
                                           (not (memq (car x)
                                                      '(list? cons? null?))))
                                      (mem (cdr l)))))))
                        ((vec-structure? srch)
                         (let mem ((l l))
                              (if (null? l)
                                  ()
                                (let ((x (car l)))
                                  (or (and (binary= (cadr x) (cadr srch))
                                           (or (disjoint? x)
                                               (vec-structure? x))
                                           (not (binary= (car x) 'vector?))
                                           (not (binary= (car x) (car srch))))
                                      (binary= x
                                              `(not (vector? ,(cadr srch))))
                                      (mem (cdr l)))))))
                        (else ())))))))
       (equal-test?
         (lambda (tst)
           (and (eq (car tst) 'binary=)
                (let ((p (caddr tst)))
                  (cond
                    ((string? p) 'string?)
                    ((boolean? p) 'boolean?)
                    ((character? p) 'character?)
                    ((number? p) 'number?)
                    ((and (cons? p)
                          (cons? (cdr p))
                          (null? (cddr p))
                          (eq 'quote (car p))
                          (symbol? (cadr p))) 'symbol?)
                    (else ()))))))
       (disjoint?
         (lambda (tst)
           (memq (car tst) match:disjoint-predicates)))
       (vec-structure?
         (lambda (tst)
           (memq (car tst) match:vector-structures)))
       (add-a
         (lambda (a)
           (let ((new (and (cons? a) (member-alist (car a) c---rs))))
             (if new (cons (cadr new) (cdr a)) `(car ,a)))))
       (add-d
         (lambda (a)
           (let ((new (and (cons? a) (member-alist (car a) c---rs))))
             (if new (cons (cddr new) (cdr a)) `(cdr ,a)))))
       (c---rs '((car caar . cdar)
                 (cdr cadr . cddr)
                 (caar caaar . cdaar)
                 (cadr caadr . cdadr)
                 (cdar cadar . cddar)
                 (cddr caddr . cdddr)
                 (caaar caaaar . cdaaar)
                 (caadr caaadr . cdaadr)
                 (cadar caadar . cdadar)
                 (caddr caaddr . cdaddr)
                 (cdaar cadaar . cddaar)
                 (cdadr cadadr . cddadr)
                 (cddar caddar . cdddar)
                 (cdddr cadddr . cddddr)))
       (setter
         (lambda (e p)
           (let ((mk-setter (lambda (s) (symbol-append 'set- s '!))))
             (cond
               ((not (cons? e))
                (match:syntax-err p "unnested setq pattern"))
               ((eq (car e) 'element)
                `(let ((x ,(cadr e)))
                   (lambda (y) (element x ,(caddr e) y))))
               ((eq (car e) 'unbox)
                `(let ((x ,(cadr e))) (lambda (y) (set-box! x y))))
               ((eq (car e) 'car)
                `(let ((x ,(cadr e))) (lambda (y) ((setter car) x y))))
               ((eq (car e) 'cdr)
                `(let ((x ,(cadr e))) (lambda (y) ((setter cdr) x y))))
               ((let ((a (member-alist (car e) get-c---rs)))
                  (and a
                       `(let ((x (,(cadr a) ,(cadr e))))
                          (lambda (y)
                            (,(mk-setter (cddr a)) x y))))))
               (else
                 `(let ((x ,(cadr e)))
                    (lambda (y) (,(mk-setter (car e)) x y))))))))
       (getter
         (lambda (e p)
           (cond
             ((not (cons? e))
              (match:syntax-err p "unnested get! pattern"))
             ((eq (car e) 'element)
              `(let ((x ,(cadr e)))
                 (lambda () (element x ,(caddr e)))))
             ((eq (car e) 'unbox)
              `(let ((x ,(cadr e))) (lambda () (unbox x))))
             ((eq (car e) 'car)
              `(let ((x ,(cadr e))) (lambda () (car x))))
             ((eq (car e) 'cdr)
              `(let ((x ,(cadr e))) (lambda () (cdr x))))
             ((let ((a (member-alist (car e) get-c---rs)))
                (and a
                     `(let ((x (,(cadr a) ,(cadr e))))
                        (lambda () (,(cddr a) x))))))
             (else
               `(let ((x ,(cadr e))) (lambda () (,(car e) x)))))))
       (get-c---rs '((caar car . car)
                     (cadr cdr . car)
                     (cdar car . cdr)
                     (cddr cdr . cdr)
                     (caaar caar . car)
                     (caadr cadr . car)
                     (cadar cdar . car)
                     (caddr cddr . car)
                     (cdaar caar . cdr)
                     (cdadr cadr . cdr)
                     (cddar cdar . cdr)
                     (cdddr cddr . cdr)
                     (caaaar caaar . car)
                     (caaadr caadr . car)
                     (caadar cadar . car)
                     (caaddr caddr . car)
                     (cadaar cdaar . car)
                     (cadadr cdadr . car)
                     (caddar cddar . car)
                     (cadddr cdddr . car)
                     (cdaaar caaar . cdr)
                     (cdaadr caadr . cdr)
                     (cdadar cadar . cdr)
                     (cdaddr caddr . cdr)
                     (cddaar cdaar . cdr)
                     (cddadr cdadr . cdr)
                     (cdddar cddar . cdr)
                     (cddddr cdddr . cdr)))
       (symbol-append
         (lambda l
           (convert
             (apply
               string-append
               (map (lambda (x)
                      (cond
                        ((symbol? x) (convert x <string>))
                        ((number? x) (convert x <string>))
                        (else x)))
                    l))
             ) <symbol>))
       (rac
         (lambda (l)
           (if (null? (cdr l))
               (car l)
             (rac (cdr l)))))
       (rdc
         (lambda (l)
           (if (null? (cdr l))
               '()
             (cons (car l) (rdc (cdr l)))))))
      (list genmatch genletrec gendefine pattern-var?)))

  ;; These are the actual macros.  They are the user-visible interface to
  ;; actual macro expanders.
  (defmacro match args
    (cond
      ((and (list? args)
            (<= 1 (size args))
            (match:andmap
              (lambda (y) (and (list? y) (<= 2 (size y))))
              (cdr args))) (let* ((exp (car args))
                                  (clauses (cdr args))
                                  (e (if (symbol? exp) exp (gensym))))
                             (if (symbol? exp)
                                 ((car match:expanders)
                                  e
                                  clauses
                                  `(match ,@args))
                               `(let ((,e ,exp))
                                  ,((car match:expanders)
                                    e
                                    clauses
                                    `(match ,@args))))))
      (else (match:syntax-err `(match ,@args) "syntax error in"))))

  (defmacro match-lambda args
    (if (and (list? args)
             (match:andmap
               (lambda (g126)
                 (if (and (cons? g126) (list? (cdr g126)))
                     (cons? (cdr g126))
                   ()))
               args))
        ((lambda ()
           (let ((e (gensym))) `(lambda (,e) (match ,e ,@args)))))
      ((lambda ()
         (match:syntax-err
           `(match-lambda ,@args)
           "syntax error in")))))

  (defmacro match-lambda* args
    (if (and (list? args)
             (match:andmap
               (lambda (g134)
                 (if (and (cons? g134) (list? (cdr g134)))
                     (cons? (cdr g134))
                   ()))
               args))
        ((lambda ()
           (let ((e (gensym))) `(lambda ,e (match ,e ,@args)))))
      ((lambda ()
         (match:syntax-err
           `(match-lambda* ,@args)
           "syntax error in")))))

  (defmacro match-let args
    (let ((g158 (lambda (pat exp body)
                  `(match ,exp (,pat ,@body))))
          (g154 (lambda (pat exp body)
                  (let ((g (map (lambda (x) (gensym)) pat))
                        (vpattern (convert pat <vector>)))
                    `(let ,(map list g exp)
                       (match (convert ,@g <vector>) (,vpattern ,@body))))))
          (g146 (lambda ()
                  (match:syntax-err `(match-let ,@args) "syntax error in")))
          (g145 (lambda (p1 e1 p2 e2 body)
                  (let ((g1 (gensym)) (g2 (gensym)))
                    `(let ((,g1 ,e1) (,g2 ,e2))
                       (match (cons ,g1 ,g2) ((,p1 . ,p2) ,@body))))))
          (g136 (cadddr match:expanders)))
      (if (cons? args)
          (if (symbol? (car args))
              (if (and (cons? (cdr args)) (list? (cadr args)))
                  (let g161 ((g162 (cadr args)) (g160 '()) (g159 '()))
                       (if (null? g162)
                           (if (and (list? (cddr args)) (cons? (cddr args)))
                               ((lambda (name pat exp body)
                                  (if (match:andmap
                                        (cadddr match:expanders)
                                        pat)
                                      `(let ,@args)
                                    `(letrec ((,name (match-lambda*
                                                       (,pat ,@body))))
                                             (,name ,@exp))))
                                (car args)
                                (reverse g159)
                                (reverse g160)
                                (cddr args))
                             (g146))
                         (if (and (cons? (car g162))
                                  (cons? (cdar g162))
                                  (null? (cddar g162)))
                             (g161 (cdr g162)
                                   (cons (cadar g162) g160)
                                   (cons (caar g162) g159))
                           (g146))))
                (g146))
            (if (list? (car args))
                (if (match:andmap
                      (lambda (g167)
                        (if (and (cons? g167)
                                 (g136 (car g167))
                                 (cons? (cdr g167)))
                            (null? (cddr g167))
                          ()))
                      (car args))
                    (if (and (list? (cdr args)) (cons? (cdr args)))
                        ((lambda () `(let ,@args)))
                      (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                           (if (null? g150)
                               (g146)
                             (if (and (cons? (car g150))
                                      (cons? (cdar g150))
                                      (null? (cddar g150)))
                                 (g149 (cdr g150)
                                       (cons (cadar g150) g148)
                                       (cons (caar g150) g147))
                               (g146)))))
                  (if (and (cons? (car args))
                           (cons? (caar args))
                           (cons? (cdaar args))
                           (null? (cddaar args)))
                      (if (null? (cdar args))
                          (if (and (list? (cdr args)) (cons? (cdr args)))
                              (g158 (caaar args)
                                    (cadaar args)
                                    (cdr args))
                            (let g149 ((g150 (car args))
                                       (g148 '())
                                       (g147 '()))
                                 (if (null? g150)
                                     (g146)
                                   (if (and (cons? (car g150))
                                            (cons? (cdar g150))
                                            (null? (cddar g150)))
                                       (g149 (cdr g150)
                                             (cons (cadar g150) g148)
                                             (cons (caar g150) g147))
                                     (g146)))))
                        (if (and (cons? (cdar args))
                                 (cons? (cadar args))
                                 (cons? (cdadar args))
                                 (null? (cdr (cdadar args)))
                                 (null? (cddar args)))
                            (if (and (list? (cdr args))
                                     (cons? (cdr args)))
                                (g145 (caaar args)
                                      (cadaar args)
                                      (caadar args)
                                      (car (cdadar args))
                                      (cdr args))
                              (let g149 ((g150 (car args))
                                         (g148 '())
                                         (g147 '()))
                                   (if (null? g150)
                                       (g146)
                                     (if (and (cons? (car g150))
                                              (cons? (cdar g150))
                                              (null? (cddar g150)))
                                         (g149 (cdr g150)
                                               (cons (cadar g150)
                                                     g148)
                                               (cons (caar g150)
                                                     g147))
                                       (g146)))))
                          (let g149 ((g150 (car args))
                                     (g148 '())
                                     (g147 '()))
                               (if (null? g150)
                                   (if (and (list? (cdr args))
                                            (cons? (cdr args)))
                                       (g154 (reverse g147)
                                             (reverse g148)
                                             (cdr args))
                                     (g146))
                                 (if (and (cons? (car g150))
                                          (cons? (cdar g150))
                                          (null? (cddar g150)))
                                     (g149 (cdr g150)
                                           (cons (cadar g150) g148)
                                           (cons (caar g150) g147))
                                   (g146))))))
                    (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                         (if (null? g150)
                             (if (and (list? (cdr args))
                                      (cons? (cdr args)))
                                 (g154 (reverse g147)
                                       (reverse g148)
                                       (cdr args))
                               (g146))
                           (if (and (cons? (car g150))
                                    (cons? (cdar g150))
                                    (null? (cddar g150)))
                               (g149 (cdr g150)
                                     (cons (cadar g150) g148)
                                     (cons (caar g150) g147))
                             (g146))))))
              (if (cons? (car args))
                  (if (and (cons? (caar args))
                           (cons? (cdaar args))
                           (null? (cddaar args)))
                      (if (null? (cdar args))
                          (if (and (list? (cdr args)) (cons? (cdr args)))
                              (g158 (caaar args)
                                    (cadaar args)
                                    (cdr args))
                            (let g149 ((g150 (car args))
                                       (g148 '())
                                       (g147 '()))
                                 (if (null? g150)
                                     (g146)
                                   (if (and (cons? (car g150))
                                            (cons? (cdar g150))
                                            (null? (cddar g150)))
                                       (g149 (cdr g150)
                                             (cons (cadar g150) g148)
                                             (cons (caar g150) g147))
                                     (g146)))))
                        (if (and (cons? (cdar args))
                                 (cons? (cadar args))
                                 (cons? (cdadar args))
                                 (null? (cdr (cdadar args)))
                                 (null? (cddar args)))
                            (if (and (list? (cdr args))
                                     (cons? (cdr args)))
                                (g145 (caaar args)
                                      (cadaar args)
                                      (caadar args)
                                      (car (cdadar args))
                                      (cdr args))
                              (let g149 ((g150 (car args))
                                         (g148 '())
                                         (g147 '()))
                                   (if (null? g150)
                                       (g146)
                                     (if (and (cons? (car g150))
                                              (cons? (cdar g150))
                                              (null? (cddar g150)))
                                         (g149 (cdr g150)
                                               (cons (cadar g150)
                                                     g148)
                                               (cons (caar g150)
                                                     g147))
                                       (g146)))))
                          (let g149 ((g150 (car args))
                                     (g148 '())
                                     (g147 '()))
                               (if (null? g150)
                                   (if (and (list? (cdr args))
                                            (cons? (cdr args)))
                                       (g154 (reverse g147)
                                             (reverse g148)
                                             (cdr args))
                                     (g146))
                                 (if (and (cons? (car g150))
                                          (cons? (cdar g150))
                                          (null? (cddar g150)))
                                     (g149 (cdr g150)
                                           (cons (cadar g150) g148)
                                           (cons (caar g150) g147))
                                   (g146))))))
                    (let g149 ((g150 (car args)) (g148 '()) (g147 '()))
                         (if (null? g150)
                             (if (and (list? (cdr args))
                                      (cons? (cdr args)))
                                 (g154 (reverse g147)
                                       (reverse g148)
                                       (cdr args))
                               (g146))
                           (if (and (cons? (car g150))
                                    (cons? (cdar g150))
                                    (null? (cddar g150)))
                               (g149 (cdr g150)
                                     (cons (cadar g150) g148)
                                     (cons (caar g150) g147))
                             (g146)))))
                (g146))))
        (g146))))

  (defmacro match-let* args
    (let ((g176 (lambda ()
                  (match:syntax-err `(match-let* ,@args) "syntax error in"))))
      (if (cons? args)
          (if (null? (car args))
              (if (and (list? (cdr args)) (cons? (cdr args)))
                  ((lambda (body) `(let* ,@args)) (cdr args))
                (g176))
            (if (and (cons? (car args))
                     (cons? (caar args))
                     (cons? (cdaar args))
                     (null? (cddaar args))
                     (list? (cdar args))
                     (list? (cdr args))
                     (cons? (cdr args)))
                ((lambda (pat exp rest body)
                   (if ((cadddr match:expanders) pat)
                       `(let ((,pat ,exp)) (match-let* ,rest ,@body))
                     `(match ,exp (,pat (match-let* ,rest ,@body)))))
                 (caaar args)
                 (cadaar args)
                 (cdar args)
                 (cdr args))
              (g176)))
        (g176))))

  (defmacro match-letrec args
    (let ((g200 (cadddr match:expanders))
          (g199 (lambda (p1 e1 p2 e2 body)
                  `(match-letrec (((,p1 . ,p2) (cons ,e1 ,e2))) ,@body)))
          (g195 (lambda ()
                  (match:syntax-err
                    `(match-letrec ,@args)
                    "syntax error in")))
          (g194 (lambda (pat exp body)
                  `(match-letrec
                     ((,(convert pat <vector>) (convert ,@exp <vector>)))
                     ,@body)))
          (g186 (lambda (pat exp body)
                  ((cadr match:expanders)
                   pat
                   exp
                   body
                   `(match-letrec ((,pat ,exp)) ,@body)))))
      (if (cons? args)
          (if (list? (car args))
              (if (match:andmap
                    (lambda (g206)
                      (if (and (cons? g206)
                               (g200 (car g206))
                               (cons? (cdr g206)))
                          (null? (cddr g206))
                        ()))
                    (car args))
                  (if (and (list? (cdr args)) (cons? (cdr args)))
                      ((lambda () `(letrec ,@args)))
                    (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                         (if (null? g190)
                             (g195)
                           (if (and (cons? (car g190))
                                    (cons? (cdar g190))
                                    (null? (cddar g190)))
                               (g189 (cdr g190)
                                     (cons (cadar g190) g188)
                                     (cons (caar g190) g187))
                             (g195)))))
                (if (and (cons? (car args))
                         (cons? (caar args))
                         (cons? (cdaar args))
                         (null? (cddaar args)))
                    (if (null? (cdar args))
                        (if (and (list? (cdr args)) (cons? (cdr args)))
                            (g186 (caaar args) (cadaar args) (cdr args))
                          (let g189 ((g190 (car args))
                                     (g188 '())
                                     (g187 '()))
                               (if (null? g190)
                                   (g195)
                                 (if (and (cons? (car g190))
                                          (cons? (cdar g190))
                                          (null? (cddar g190)))
                                     (g189 (cdr g190)
                                           (cons (cadar g190) g188)
                                           (cons (caar g190) g187))
                                   (g195)))))
                      (if (and (cons? (cdar args))
                               (cons? (cadar args))
                               (cons? (cdadar args))
                               (null? (cdr (cdadar args)))
                               (null? (cddar args)))
                          (if (and (list? (cdr args)) (cons? (cdr args)))
                              (g199 (caaar args)
                                    (cadaar args)
                                    (caadar args)
                                    (car (cdadar args))
                                    (cdr args))
                            (let g189 ((g190 (car args))
                                       (g188 '())
                                       (g187 '()))
                                 (if (null? g190)
                                     (g195)
                                   (if (and (cons? (car g190))
                                            (cons? (cdar g190))
                                            (null? (cddar g190)))
                                       (g189 (cdr g190)
                                             (cons (cadar g190) g188)
                                             (cons (caar g190) g187))
                                     (g195)))))
                        (let g189 ((g190 (car args))
                                   (g188 '())
                                   (g187 '()))
                             (if (null? g190)
                                 (if (and (list? (cdr args))
                                          (cons? (cdr args)))
                                     (g194 (reverse g187)
                                           (reverse g188)
                                           (cdr args))
                                   (g195))
                               (if (and (cons? (car g190))
                                        (cons? (cdar g190))
                                        (null? (cddar g190)))
                                   (g189 (cdr g190)
                                         (cons (cadar g190) g188)
                                         (cons (caar g190) g187))
                                 (g195))))))
                  (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                       (if (null? g190)
                           (if (and (list? (cdr args)) (cons? (cdr args)))
                               (g194 (reverse g187)
                                     (reverse g188)
                                     (cdr args))
                             (g195))
                         (if (and (cons? (car g190))
                                  (cons? (cdar g190))
                                  (null? (cddar g190)))
                             (g189 (cdr g190)
                                   (cons (cadar g190) g188)
                                   (cons (caar g190) g187))
                           (g195))))))
            (if (cons? (car args))
                (if (and (cons? (caar args))
                         (cons? (cdaar args))
                         (null? (cddaar args)))
                    (if (null? (cdar args))
                        (if (and (list? (cdr args)) (cons? (cdr args)))
                            (g186 (caaar args) (cadaar args) (cdr args))
                          (let g189 ((g190 (car args))
                                     (g188 '())
                                     (g187 '()))
                               (if (null? g190)
                                   (g195)
                                 (if (and (cons? (car g190))
                                          (cons? (cdar g190))
                                          (null? (cddar g190)))
                                     (g189 (cdr g190)
                                           (cons (cadar g190) g188)
                                           (cons (caar g190) g187))
                                   (g195)))))
                      (if (and (cons? (cdar args))
                               (cons? (cadar args))
                               (cons? (cdadar args))
                               (null? (cdr (cdadar args)))
                               (null? (cddar args)))
                          (if (and (list? (cdr args)) (cons? (cdr args)))
                              (g199 (caaar args)
                                    (cadaar args)
                                    (caadar args)
                                    (car (cdadar args))
                                    (cdr args))
                            (let g189 ((g190 (car args))
                                       (g188 '())
                                       (g187 '()))
                                 (if (null? g190)
                                     (g195)
                                   (if (and (cons? (car g190))
                                            (cons? (cdar g190))
                                            (null? (cddar g190)))
                                       (g189 (cdr g190)
                                             (cons (cadar g190) g188)
                                             (cons (caar g190) g187))
                                     (g195)))))
                        (let g189 ((g190 (car args))
                                   (g188 '())
                                   (g187 '()))
                             (if (null? g190)
                                 (if (and (list? (cdr args))
                                          (cons? (cdr args)))
                                     (g194 (reverse g187)
                                           (reverse g188)
                                           (cdr args))
                                   (g195))
                               (if (and (cons? (car g190))
                                        (cons? (cdar g190))
                                        (null? (cddar g190)))
                                   (g189 (cdr g190)
                                         (cons (cadar g190) g188)
                                         (cons (caar g190) g187))
                                 (g195))))))
                  (let g189 ((g190 (car args)) (g188 '()) (g187 '()))
                       (if (null? g190)
                           (if (and (list? (cdr args)) (cons? (cdr args)))
                               (g194 (reverse g187)
                                     (reverse g188)
                                     (cdr args))
                             (g195))
                         (if (and (cons? (car g190))
                                  (cons? (cdar g190))
                                  (null? (cddar g190)))
                             (g189 (cdr g190)
                                   (cons (cadar g190) g188)
                                   (cons (caar g190) g187))
                           (g195)))))
              (g195)))
        (g195))))

  (defmacro match-define args
    (let ((g210 (cadddr match:expanders))
          (g209 (lambda ()
                  (match:syntax-err
                    `(match-define ,@args)
                    "syntax error in"))))
      (if (cons? args)
          (if (g210 (car args))
              (if (and (cons? (cdr args)) (null? (cddr args)))
                  ((lambda () `(progn (deflocal ,@args))))
                (g209))
            (if (and (cons? (cdr args)) (null? (cddr args)))
                ((lambda (pat exp)
                   ((caddr match:expanders)
                    pat
                    exp
                    `(match-define ,@args)))
                 (car args)
                 (cadr args))
              (g209)))
        (g209))))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Testing
;;;-----------------------------------------------------------------------------

(defun mymap (f l)
  (match l
         (() ())
         ((x . y) (cons (f x) (mymap f y)))))

(defun match-proc args
  (cond
    ((and (list? args)
          (<= 1 (size args))
          (match:andmap
            (lambda (y) (and (list? y) (<= 2 (size y))))
            (cdr args))) (let* ((exp (car args))
                                (clauses (cdr args))
                                (e (if (symbol? exp) exp (gensym))))
                           (if (symbol? exp)
                               ((car match:expanders)
                                e
                                clauses
                                `(match ,@args))
                             `(let ((,e ,exp))
                                ,((car match:expanders)
                                  e
                                  clauses
                                  `(match ,@args))))))
    (else (match:syntax-err `(match ,@args) "syntax error in"))))
