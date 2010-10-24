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
;;; Title: code generation
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule cg-gen
  (syntax (_macros
           _i-aux0)
   import (i-all
           cg-state
           cg-stack
           sx-obj
           sx-node
           p-env
           an-side)
   export (encode
           interactive-encode))

;;;-----------------------------------------------------------------------------
;;; Generate assembler code
;;;-----------------------------------------------------------------------------
(defgeneric encode (node state))

(defmethod encode (n (state <null>))
  (with-ct-handler "code generation error" n
                   (notify "  Encoding module ~a ..."
                           (module-name? (dynamic *actual-module*)))
                   (setq *pass* 'encode)
                   (let ((new-state (make-code-state)))
                     (encode n new-state)
                     (code-state-asm! new-state (reverse (code-state-asm? new-state)))
                     new-state)))

(defun interactive-encode (module)
  (with-ct-handler "code generation error" module
                   (notify "  Encoding module ~a ..." (module-name? module))
                   (setq *pass* 'encode)
                   (let ((state (make-code-state)))
                     (compute-captured-vars module)
                     (interactive-encode-top-lexical-bindings module state)
                     (notify0 "  Code for ~a's top-level forms:" (module-name? module))
                     (encode (module-top-level-forms? module) state)
                     (code-state-asm! state (reverse (code-state-asm? state)))
                     state)))

;;;-----------------------------------------------------------------------------
;;; Generate code for MODULE node
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <module>) (state <code-state>))
  (compute-captured-vars n)
  (encode-top-lexical-bindings n state)
  (notify0 "  Code for ~a's top-level forms:" (module-name? n))
  (encode (module-top-level-forms? n) state)
  state)

(defun encode-top-lexical-bindings (module state)
  (do1-list pre-encode-inlined-lambda (module-inlined-lambdas? module))
  (do1-list pre-encode-inlined-setter (module-inlined-setters? module))
  (access-table-do
   (lambda (name binding)
     ;; Attention -- name is ptr to C string!
     (and (true-local-binding? binding)
          (encode-top-lexical-binding binding state)))
   (module-lexical-env? module)))

(defun interactive-encode-top-lexical-bindings (module state)
  (access-table-do (lambda (name binding)
                     (and (true-local-binding? binding)
                          (encode-top-lexical-binding binding state)))
                   (module-interactive-lexical-env? module)))

(defun encode-top-lexical-binding (binding state)
  (let ((obj (binding-obj? binding)))
    (notify0 "  Code for module binding ~a:" (local-name? binding))
    (encode obj state)
    (add-asm-set-binding binding state)))

(defun pre-encode-inlined-setter (binding)
  (if (interface-binding? binding) ()
    (progn
      (notify0 "  Pre-encode inlined setter ~a" (local-name? binding))
      (pre-encode-inlined-lambda binding)
      (let* ((host-binding-name (cadr (binding-local-name? binding)))
             (host-binding (get-lexical-binding host-binding-name))
             (inline-spec
              `(setter . ,(get-binding-info binding 'inline)))
             (info (binding-info? host-binding)))
        (binding-info! host-binding (append info (list inline-spec)))
        (notify0 "new inlined setter info: ~a"
                 (binding-info? host-binding))))))

(defun pre-encode-inlined-lambda (binding)
  (let* ((fun (binding-obj? binding))
         (body (fun-body? fun))
         (args (fun-args? fun))
         (nargs (list-size args))
         (state (make-code-state (binding-local-name? binding)))
         ;; remember stack pos to check how many args need to be checked;
         ;; nargs cannot be used as appls may consume consts and vars
         (pos (code-state-stack-size? state)))
    (notify0 "  Pre-encode inlined lambda ~a" (local-name? binding))
    (do1-list (lambda (var) (push-stack-var var state)) args)
    (encode (fun-body? fun) state)
    (clear-stack pos state)
    (code-state-asm! state (reverse (code-state-asm? state)))
    (set-inline-binding-info binding state)))

;;;-----------------------------------------------------------------------------
;;; Generate code for BINDING node
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <binding>) (state <code-state>))
  (add-asm-get-binding n state))

;;;-----------------------------------------------------------------------------
;;; Generate code for LITERAL nodes
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <literal-const>) (state <code-state>))
  (encode-literal (const-value? n) state))

(defgeneric encode-literal (n state))

(defmethod encode-literal ((n <object>) (state <code-state>))
  (if (eq n t)
      (add-asm `(static-ref-t) 1 state)
    (add-asm `(static-ref ,n) 1 state)))

(defmethod encode-literal ((n <null>) (state <code-state>))
  (add-asm `(static-ref-nil) 1 state))

(defmethod encode-literal ((n <character>) (state <code-state>))
  (let ((ascii-value (character-as-fpi n)))
    (add-asm `(static-character-ref ,ascii-value) 1 state)))

(defmethod encode-literal ((n <fpi>) (state <code-state>))
  (cond ((= n 0) (add-asm `(static-ref0) 1 state))
        ((= n 1) (add-asm `(static-ref1) 1 state))
        ((= n 2) (add-asm `(static-ref2) 1 state))
        ((= n -1) (add-asm `(static-ref-1) 1 state))
        ((and (< -128 n) (< n 128))
         (add-asm `(static-fpi-byte-ref ,n) 1 state))
        (t (add-asm `(static-fpi-ref ,n) 1 state))))

;;;-----------------------------------------------------------------------------
;;; Generate code for NAMED CONSTANT node
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <named-const>) (state <code-state>))
  (encode (const-value? n) state))

;;;-----------------------------------------------------------------------------
;;; Generate code for VARIABLE node
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <global-static-var>) (state <code-state>))
  (encode (var-value? n) state))

;;;-----------------------------------------------------------------------------
;;; Generate code for SETQ node
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <setq>) (state <code-state>))
  (let* ((binding (setq-binding? n)))
    (encode (setq-obj? n) state)
    (add-asm-set-and-get-binding binding state)))

;;;-----------------------------------------------------------------------------
;;; Generate code for IF node
;;; Thanks to A-normalization always in tail position, i.e. no end label.
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <if>) (state <code-state>))
  (let ((else-label (gensym-module))
        (out-label (gensym-module))
        (tmp-stack-size (code-state-stack-size? state))
        (tmp-stack-vars (code-state-stack-vars? state)))
    (notify0 "    Gen code for if")
    (encode (if-pred? n) state)
    (add-asm `(branch-nil ,else-label) -1 state)
    (encode (if-then? n) state)
    (add-asm `(branch ,out-label) 0 state)
    ;; reset the state stack for else branch
    (code-state-stack-size! state tmp-stack-size)
    (code-state-stack-vars! state tmp-stack-vars)
    (add-asm `(label ,else-label) 0 state)
    (encode (if-else? n) state)
    (add-asm `(label ,out-label) 0 state)))

(defun gensym-module ()
  (make-symbol (fmt "~a~a"
                    (gensym)
                    (module-name? (dynamic *actual-module*)))))

;;;-----------------------------------------------------------------------------
;;; Generate code for RETURN node
;;;-----------------------------------------------------------------------------
;  (defmethod encode ((n <return>) (state <code-state>))
;    (let ((arity (abs (fun-arity? (syntax-expr-encl-lambda? n)))))
;      (encode (return-form? n) state)
;      (add-asm `(return ,arity) 0 state)))

;;;-----------------------------------------------------------------------------
;;; Generate code for LAMBDA node
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <lambda>) (state <code-state>))
  (notify0 "  Gen code for <lambda>")
  (let* ((name (or (local-name? n) (fun-name? n)))
         (args (fun-args? n))
         (arity (fun-arity? n))
         (body (fun-body? n))
         (lstate (make-code-state name))
         ;; remember stack pos to check how many args need to be checked;
         ;; nargs cannot be used as appls may consume consts and vars
         (pos (code-state-stack-size? lstate)))
    ;; link lstate and state
    (code-state-enclosed-code!
     state (cons lstate (code-state-enclosed-code? state)))
    (code-state-display! lstate (code-state-display? state))
    (notify0 "    Args are on stack ~a" (map1-list var-name? args))
    (do1-list (lambda (var) (push-stack-var var lstate)) args)
    (encode-check-arguments arity lstate)
    (notify0 "    Gen code for lambda display")
    (encode-lambda-display n args lstate)
    (notify0 "    Gen code for lambda body")
    (dynamic-let ((*in-tail-pos* t)) (encode (fun-body? n) lstate))
    (encode-return pos lstate)
    (code-state-asm! lstate (reverse (code-state-asm? lstate)))
    ;; Register static bytevector
    (notify0 "    Gen code for lambda naming")
    (add-asm `(static-ref ,name) 1 state)
    (add-asm `(code-vector-ref ,(code-state-handle? lstate)) 1 state)
    ;;(add-asm `(make-lambda ,(abs arity)) -1 state)
    (add-asm `(make-lambda ,arity) -1 state)))

(defun encode-check-arguments (n state)
  (cond ((= n -2) (add-asm `(check-arguments-2) 0 state))
        ((= n -1) (add-asm `(check-arguments-1) 0 state))
        ((= n 0) (add-asm `(check-arguments0) 0 state))
        ((= n 1) (add-asm `(check-arguments1) 0 state))
        ((= n 2) (add-asm `(check-arguments2) 0 state))
        (t (add-asm `(check-arguments ,n) 0 state))))

(defun encode-lambda-display (n args state)
  ;; Copy captured args into display
  (let* ((local-vars (select-list local-static-var-captured? args))
         (delegated-vars
          (select-list local-static-var-captured?
                       (lambda-delegated-vars? n)))
         (display-size (+ (list-size local-vars)
                          (list-size delegated-vars))))
    (notify0 "    Captured local vars ~a" local-vars)
    (notify0 "    Captured delegated vars ~a" delegated-vars)
    (if (or local-vars delegated-vars)
        (progn
          (push-display (append local-vars delegated-vars) state)
          (add-asm `(alloc ,display-size) 0 state)
          ;; move arguments
          (do1-list (lambda (var) (stack-to-display var state)) local-vars))
      ())))

(defun encode-return (pos state)
  (let ((to-pop (- (code-state-stack-size? state) (+ pos 1))))
    (add-asm `(return ,to-pop) (- 0 to-pop) state)))

;;;-----------------------------------------------------------------------------
;;; Generate code for LET* node
;;  Treated as nested inlined lambda application with arity=1.
;;  Init forms are not in tail positon (see A-normalization).
;;  Init forms are stored as var values.
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <let*>) (state <code-state>))
  (notify0 "  Gen code for ~a" n)
  (let* ((args (fun-args? n))
         (body (fun-body? n))
         (nargs (list-size args))
         ;; remember stack pos to check how many args need to be checked;
         ;; nargs cannot be used as appls may consume consts and vars
         (pos (code-state-stack-size? state)))
    (labels ((loop (args)
                   (let ((first-arg (car args))
                         (rest-args (cdr args)))
                     (dynamic-let ((*in-tail-pos* ()))
                                  (encode (var-value? first-arg) state))
                     (if (= (var-used? first-arg) 0)
                         (add-asm '(pop1) -1 state)
                       (progn
                         (move-stack -1 state)
                         (push-stack-var first-arg state)
                         (and (local-static-var-captured? first-arg)
                              (stack-to-display first-arg state))))
                     (if (null? rest-args)
                         (progn
                           (notify0 "  Gen code for let body ~a" n)
                           (encode body state))
                       (loop rest-args)))))
            (loop args)
            (clear-stack pos state))))

;;;-----------------------------------------------------------------------------
;;; Generate code for APPL node
;;;-----------------------------------------------------------------------------
(defglobal *in-tail-pos* t)

(defmethod encode ((n <appl>) (state <code-state>))
  (notify0 "  Gen code for <appl>")
  (let* ((args (appl-args? n))
         (nargs (list-size args))
         (op (appl-fun? n)))
    (encode-args args nargs state)
    (cond
      ;;--------------------------
      ;; Call init lambda with delegated vars
      ;; Thanks to A-normalization lambdas in operator position do not
      ;; occur; only exception is the init lambda which is not normalized
      ;; (see ex-body).
      ;;--------------------------
      ((and (lambda? op) (lambda-delegated-vars? op))
       (notify0 "call init lambda ~a" op)
       (encode op state)
       (if (dynamic *in-tail-pos*)
           (add-asm `(tail-call-operator
                      0 ,(- (code-state-stack-size? state) 1)) 0 state)
         (add-asm `(call-operator 0) 0 state)))
      ;;--------------------------
      ;; Call inlined lambda (let) or init lambda without delegated vars.
      ;;--------------------------
      ((lambda? op)
       (notify0 "call let lambda ~a" op)
       (encode-let op args nargs state))
      ;;--------------------------
      ;; Call opencoding (opencoded-lambda)
      ;;--------------------------
      ((opencoding? op)
       (notify0 "call opencoded-lambda ~a" op)
       (add-asms (fun-body? op) (- 1 nargs) state))
      ;;--------------------------
      ;; Call binding to ...
      ;;--------------------------
      ((binding? op)
       (notify0 "call binding ~a" (binding-local-name? op))
       (let ((obj (binding-obj? op))
             (binding-name (binding-local-name? op))
             (binding-class (get-binding-info op 'class)))
         (cond
           ;;--------------------------
           ;; ... inlined binding (declare-inline, inlined setter)
           ;;--------------------------
           ((get-binding-info op 'inline)
            (notify0 "call inlined binding ~a" binding-name)
            (add-inline-binding-info op state)
            (move-stack (- 0 (- nargs 1)) state))
           ;;--------------------------
           ;; ... opencoding (defopencoding)
           ;;--------------------------
           ((eq binding-class 'opencoding)
            (and (null? (= nargs (get-binding-info op 'arity)))
                 (ct-serious-warning t "argument mismatch at opencoding ~a"
                                     binding-name))
            (add-asms (get-binding-info op 'opencoding) (- 1 nargs) state))
           ;;--------------------------
           ;; ... ff
           ;;--------------------------
           ((eq binding-class 'ff)
            (and (null? (= nargs (get-binding-info op 'arity)))
                 (ct-serious-warning
                  t "argument mismatch at foreign function ~a" binding-name))
            (add-asm `(call-foreign-function ,binding-name) 1 state)
            (and (< 0 nargs)
                 (add-asm `(nobble ,nargs) (- 0 nargs) state)))
           ;;--------------------------
           ;; ... other binding
           ;;--------------------------
           (t
            (notify0 "call to general binding")
            (encode op state)
            (if (dynamic *in-tail-pos*)
                ;; Previous params are now obsolet
                (add-asm `(tail-call-operator
                           ,nargs
                           ,(- (code-state-stack-size? state) (+ nargs 1)))
                         (- 0 nargs) state)
              (add-asm `(call-operator ,nargs) (- 0 nargs) state))))))
      (t
       (ct-serious-warning () "bad call to ~a" op)))))

(defun encode-args (args nargs state)
  (labels
   ;; devide args into 2 sets; args of the first set can be consumed
   ;; from the stack; args of second set are put onto the stack
   ((devide-args (l rest)
                 (if (null? l) (cons () rest)
                   (let* ((arg (car l))
                          (obj (if (binding? arg) (binding-obj? arg) ())))
                     (cond ((const? arg)
                            (devide-args (cdr l) (cons arg rest)))
                           ((null? (binding? arg))
                            (cons (reverse l) rest))
                           ((local-static-var? obj)
                            (cons (reverse l) rest))
                           (t
                            (devide-args (cdr l) (cons arg rest)))))))
    ;; check if the args which can be consumed from the stack are in
    ;; correct position for the function application
    (stack-args-in-position (l i)
                            (if (null? l) t
                              (let* ((arg (car l))
                                     (obj (if (binding? arg) (binding-obj? arg) ())))
                                (cond ((const? arg)
                                       ())
                                      ((null? (binding? arg))
                                       ())
                                      ((and (local-static-var? obj)
                                            (< (var-used? obj) 2)
                                            (null? (local-static-var-captured? obj))
                                            (= (stack-var-index obj state) i))
                                       (stack-args-in-position (cdr l) (- i 1))))))))
   (let* ((devided-args (devide-args (reverse args) ()))
          (stack-args (car devided-args))
          (new-args (cdr devided-args)))
     (if (stack-args-in-position stack-args
                                 (- nargs (+ (list-size new-args) 1)))
         (do1-list (lambda (arg) (encode arg state)) new-args)
       (do1-list (lambda (arg) (encode arg state)) args)))))

(defun encode-let (op args nargs state)
  (move-stack (- 0 nargs) state)
  ;; remember stack pos to check how many args need to be checked;
  ;; nargs cannot be used as appls may consume consts and vars
  (let ((pos (code-state-stack-size? state)))
    (do1-list (lambda (var) (push-stack-var var state)) (fun-args? op))
    (do1-list (lambda (var)
                (and (local-static-var-captured? var)
                     (stack-to-display var state)))
              (fun-args? op))
    (encode (fun-body? op) state)
    (clear-stack pos state)))

;;;-----------------------------------------------------------------------------
;;; Generate code for OPENCODING node
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <opencoding>) (state <code-state>))
  (notify0 "  Gen code for <opencoding>")
  ;; do nothing
  state)

;;;-----------------------------------------------------------------------------
;;; Generate code for CALL-NEXT-METHOD node
;;;-----------------------------------------------------------------------------
(defmethod encode ((n <call-next-method>) (state <code-state>))
  (let* ((method-lambda (syntax-expr-encl-lambda? n))
         (arity (abs (fun-arity? method-lambda)))
         (bindings (map1-list var-binding? (fun-args? method-lambda))))
    (if (dynamic *in-tail-pos*)
        (add-asm `(tail-call-next-method
                   ,arity
                   ,(- (code-state-stack-size? state) arity)) 1 state)
      (progn
        (do1-list (lambda (binding) (add-asm-get-binding binding state))
                  bindings)
        (add-asm `(call-next-method ,arity) (- 1 arity) state)))))

;;;-----------------------------------------------------------------------------
;;;  Binding access; resolve instructions and parameters to be used.
;;;-----------------------------------------------------------------------------
(defun add-asm-get-binding (binding state)
  (let ((values (and (eq (get-binding-info binding 'class) 'constant)
                     (get-binding-info binding 'value))))
    (if values  ; const folding?
        (encode (make <literal-const> value: (car values)) state)
      (add-asm `(,(binding-read-instr binding)
                 ,@(binding-access-params binding state)) 1 state))))

(defun add-asm-set-binding (binding state)
  (add-asm `(,(binding-write-instr binding)
             ,@(binding-access-params binding state)) -1 state))

(defun add-asm-set-and-get-binding (binding state)
  (let ((obj (binding-obj? binding))
        (name (binding-local-name? binding))
        (module-name (save-binding-module-name? binding)))
    (cond ((interface-binding? binding)
           (add-asm `(set-and-get-binding-ref ,module-name ,name)
                    0 state))
          ((or (null? obj) (local-static-var? obj))
           (add-asm-set-binding binding state)
           (add-asm-get-binding binding state))
          (t
           (add-asm `(set-and-get-binding-ref ,module-name ,name)
                    0 state)))))

(defun binding-read-instr (binding)
  (let ((obj (binding-obj? binding)))
    (cond ((interface-binding? binding)
           'binding-ref)
          ((null? obj) 'noop)
          ((local-static-var? obj)
           (if (local-static-var-captured? obj)
               'display-ref
             'stack-ref))
          (t 'binding-ref))))

(defun binding-write-instr (binding)
  (let ((obj (binding-obj? binding)))
    (cond ((interface-binding? binding)
           'set-binding-ref)
          ((null? obj) 'noop)
          ((local-static-var? obj)
           (if (local-static-var-captured? obj)
               'set-display-ref
             'set-stack-ref))
          (t 'set-binding-ref))))

(defun binding-access-params (binding state)
  (let ((obj (binding-obj? binding))
        (name (binding-local-name? binding))
        (module-name (save-binding-module-name? binding)))
    (cond ((interface-binding? binding)
           (list module-name name))
          ((null? obj)
           (notify0 "binding-access-params: unknown obj type ~a" obj)
           (ct-error () "can't generate code for dummy binding"))
          ((local-static-var? obj)
           (if (local-static-var-captured? obj)
               (display-var-index obj state)
             (list (stack-var-index obj state))))
          (t
           (list module-name name)))))

;;;-----------------------------------------------------------------------------
;;; Move stack-ref to display
;;;-----------------------------------------------------------------------------
(defun stack-to-display (var state)
  (add-asm `(stack-ref ,(stack-var-index var state)) 1 state)
  (add-asm `(set-display-ref ,@(display-var-index var state)) -1 state))

;;;-----------------------------------------------------------------------------
;;;  Add code to code state object
;;;-----------------------------------------------------------------------------
(defun add-asm (code move state)
  (and (null? (car code))
       (ct-error () "error in code generation"))
  (move-stack move state)
  ;; Code has to be reversed later
  (code-state-asm! state (cons code (code-state-asm? state))))

(defun add-asms (asms move state)
  ;; These asms must be in correct order
  (let ((prefix (symbol-name (gensym)))
        (last-asm ()))
    (do1-list (lambda (asm)
                (setq last-asm asm)
                ;; Make labels distinct
                (if (member1-list (car asm)
                                  '(label branch branch-nil branch-true))
                    (let* ((label-name (car (cdr asm)))
                           (new-label-name
                            (make-symbol
                             (string-append prefix
                                            (symbol-name label-name)))))
                      (add-asm (list (car asm) new-label-name) 0 state))
                  (add-asm asm 0 state)))
              asms)
    ;; Change last tail call into simple call if necessary;
    ;; because we never know where asms are inlined
    (let ((key (car last-asm)))
      (if (member key '(tail-call-operator tail-call-next-method))
          (let ((nargs (car (cdr last-asm)))
                (topop (car (cdr (cdr last-asm))))
                (new-key (if (eq key 'tail-call-operator)
                             'call-operator 'call-next-method)))
            ((setter car) last-asm new-key)
            ((setter cdr) last-asm (list nargs))
            (add-asm `(nobble ,topop) 0 state))
        ()))
    ;; Now do the stack movement in one step
    (move-stack move state)))

(defun set-inline-binding-info (binding state)
  (notify0 "  Set inlined binding info of ~a" (local-name? binding))
  (let ((new-info-entry
         `(inline ,@(map1-list (lambda (s)
                                 `(,(code-state-handle? s) .
                                   ,(code-state-asm? s)))
                               (cons state
                                     (code-state-enclosed-code? state)))))
        (old-info (binding-info? binding)))
    (binding-info! binding (cons new-info-entry old-info))))

(defun add-inline-binding-info (binding state)
  (let* ((inline-spec (get-binding-info binding 'inline))
         (lstates (map1-list (lambda (x)
                               (make <code-state>
                                     handle: (car x)
                                     asm: (cdr x)))
                             (cdr inline-spec))))  ; not first state
    (code-state-enclosed-code!
     state (append lstates
                   (code-state-enclosed-code? state)))
    (add-asms (cdr (car inline-spec)) 0 state)))

(defun clear-stack (pos state)
  (let ((to-pop (- (code-state-stack-size? state) (+ pos 1))))
    (if (< 0 to-pop)
        (add-asm `(nobble ,to-pop) (- 0 to-pop) state)
      ())))

;;;-----------------------------------------------------------------------------
)  ;; End of module cg-gen
;;;-----------------------------------------------------------------------------
