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
;;; Title: create nodes of the abstract syntax tree
;;;  Library: eval (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule sx-node
  (syntax (_syntax-1
           _i-aux0
           _sx-obj0)
   import (i-all
           i-ffi
           sx-obj
           p-env)
   export (make-module
           make-defined-fun
           make-defined-opencoded-fun
           make-defined-external-fun
           make-fun
           make-let*
           make-setq
           make-inlined-setter
           get-inlined-setter-binding
           make-named-const
           make-global-var
           make-local-static-var
           make-immutable-binding
           make-mutable-binding
           make-dummy-binding
           binding-origin-module-name
           get-binding-info
           get-binding-spec-info
           true-local-binding?
           non-folded-local-binding?
           compute-arity
           register-delegated-vars
           register-binding-ref
           clone-node))

;;;-----------------------------------------------------------------------------
;;;  Create module node
;;;-----------------------------------------------------------------------------
(defun make-module (name)
  (let ((module (make <module>
                      name: name
                      lexical-env: (make-access-table)
                      interactive-lexical-env: (make-access-table)
                      external-env: (make-access-table)
                      syntax-env: (make-access-table))))
    (new-module name module)
    (module-load-dir! module *tmp-load-dir*)
    module))

;;;-----------------------------------------------------------------------------
;;; Create setq node
;;;-----------------------------------------------------------------------------
(defun make-setq (binding node)
  (make <setq> binding: binding obj: node))

;;;-----------------------------------------------------------------------------
;;; Create inlined setter node
;;;-----------------------------------------------------------------------------
(defun make-inlined-setter (name args body)
  (let* ((node (make-fun <lambda> name args body))
         (binding (make-immutable-binding node)))
    (lambda-inlined! node t)
    (new-node binding 'inlined-setter)
    node))

(defun get-inlined-setter-binding (setter-name env)
  (let ((name (cadr setter-name)))
    (labels
     ((loop (l)
            (and l
                 (if (eq (cadr (binding-local-name? (car l))) name)
                     (car l)
                   (loop (cdr l))))))
     (or (get-imported-inlined-setter-binding setter-name env)
         (loop (module-inlined-setters? (dynamic *actual-module*)))))))

(defun get-imported-inlined-setter-binding (setter-name env)
  (let* ((name (cadr setter-name))
         (host-binding (or (get-lexical-binding name)
                           (get-local-static-binding name env)
                           (ct-serious-warning
                            (make-dummy-binding name)
                            "no lexical binding ~a available" name)))
         (setter-spec (get-binding-info host-binding 'setter)))
    (and setter-spec
         (let ((binding (make <interface-binding>
                              local-name: setter-name
                              module: (binding-module? host-binding)
                              immutable: t
                              obj: setter-name
                              imported: t
                              info: `((inline . ,setter-spec))))
               (module (dynamic *actual-module*)))
           (module-inlined-setters!
            module (cons binding (module-inlined-setters? module)))
           binding))))

;;;-----------------------------------------------------------------------------
;;; Create defined constant node
;;;-----------------------------------------------------------------------------
(defun make-named-const (name value)
  (let* ((node (make <named-const> name: name value: value))
         (binding (make-immutable-binding node)))
    (if (and (null? *interpreter*) (foldable-constant? value))
        ;; Suppress constant folding with interpreter
        (let ((info-entries `((class . constant) (value ,value))))
          (binding-info! binding (append info-entries
                                         (binding-info? binding))))
      (let ((info-entries '((class . constant))))
        (binding-info! binding (append info-entries
                                       (binding-info? binding)))))
    ;; Always create a node to store the constant even if it is foldable.
    ;; This makes the constant available in the interpreter as an
    ;; external binding rather than a foldable (inline) binding which are not
    ;; currently available in the interpreter.
    (new-node node 'named-constant t)
    (set-lexical-binding binding)
    node))

(defun foldable-constant? (x)
  ;; what about names?
  (or (number? x)
      (character? x)
      (null? x)))

;;;-----------------------------------------------------------------------------
;;; Create global static var (deflocal)
;;;-----------------------------------------------------------------------------
(defun make-global-var (name value)
  (let ((node (make <global-static-var> name: name value: value used: 0)))
    (set-lexical-binding (make-mutable-binding node))
    (new-node node 'static-variable t)
    node))

;;;-----------------------------------------------------------------------------
;;; Create local static var (let, let*, formal parameters)
;;;-----------------------------------------------------------------------------
(defun make-local-static-var (name . value)
  (let* ((v (and value (car value)))
         (node (make <local-static-var> name: name value: v used: 0)))
    (make-mutable-binding node)
    node))

;;;-----------------------------------------------------------------------------
;;; Create defined function node
;;;-----------------------------------------------------------------------------
(defun make-defined-fun (name args body)
  (let ((node (make-fun <lambda> name args body)))
    (set-lexical-binding (make-immutable-binding node))
    node))

(defun make-fun (fun-class name args body . has-unknown-appls)
  (let* ((special-name? (and (cons? name) (= (list-size name) 1)))
         (node (make fun-class
                     name: (if special-name? (car name) name)
                     args: (if (symbol? args) (list args) args)
                     arity: (compute-arity args)
                     body: (or (syntax-obj? body) `(progn ,@body))
                     has-unknown-appls: (and has-unknown-appls
                                             (car has-unknown-appls)))))
    (and (lambda? node)
         (if (or (eq name 'anonymous) special-name?)
             (new-node node 'anonymous-lambda)
           (new-node node 'named-lambda)))
    node))

(defun compute-arity (params)
  (if (proper-list? params)
      (list-size params)
    (if (atom? params)
        -1
      (- 0 (+ (list-size params) 1)))))

;;;-----------------------------------------------------------------------------
;;; Create let* node
;;;-----------------------------------------------------------------------------
(defun make-let* (vars body)
  (let ((n (make-fun <let*> 'anonymous vars body)))
    (do1-list (lambda (var)
                (local-static-var-lambda! var n)) vars)
    (register-delegated-vars vars)
    n))

;;;-----------------------------------------------------------------------------
;;; Create opencoded function node
;;;-----------------------------------------------------------------------------
(defun make-defined-opencoded-fun (name args body)
  (let ((binding (make-immutable-binding () name))
        (arity (compute-arity args)))
    (set-lexical-binding binding)
    (binding-info! binding `((class . opencoding)
                             (arity . ,arity)
                             (opencoding . ,body)))
    binding))

;;;-----------------------------------------------------------------------------
;;; Create opencoded function node
;;;-----------------------------------------------------------------------------
(defun make-defined-external-fun (name1 arg-convs res-conv name2)
  (let* ((arg-conv-codes (map1-list arg-converter-index arg-convs))
         (res-conv-code (res-converter-index res-conv))
         (binding (make-immutable-binding () name1))
         (arity (compute-arity arg-convs))
         (ext-name (or (and name2 (car name2)) (symbol-name name1))))
    (set-lexical-binding binding)
    (binding-info! binding `((class . ff)
                             (arity . ,arity)
                             (ff ,arg-conv-codes
                                 ,res-conv-code
                                 ,ext-name)))
    (new-node binding 'foreign-function)
    binding))

;;;-----------------------------------------------------------------------------
;;; Create binding
;;;-----------------------------------------------------------------------------
(defun make-immutable-binding (node . name)
  (let ((binding-name (or (and name (car name)) (slot-value node 'name))))
    (make-binding node binding-name t)))

(defun make-mutable-binding (node . name)
  (let ((binding-name (or (and name (car name)) (slot-value node 'name))))
    (make-binding node binding-name ())))

(defun make-binding (node name . immutable)
  (let ((binding (make <binding>
                       local-name: name
                       module: (dynamic *actual-module*)
                       immutable: (and immutable (car immutable))
                       obj: node)))
    (and (syntax-obj? node)
         (binding! node binding))
    binding))

(defun make-dummy-binding names
  (make <binding>
        local-name: (or (and names (car names)) '| unbound |)
        module: (dynamic *actual-module*)))

;; Hack: later to be removed
;  (defun make-macro-binding (node name)
;    (make <binding>
;         local-name: name
;         module: (dynamic *actual-module*)
;         obj: node))

(defun true-local-binding? (binding)
  (if (binding? binding)
      (let ((obj (binding-obj? binding)))
        (null? (or (interface-binding? binding)  ; from interface file
                   (binding-imported? binding)    ; from just compiled module
                   (opencoding? obj)
                   (get-binding-info binding 'opencoding)
                   (get-binding-info binding 'ff)
                   (and (eq (get-binding-info binding 'class) 'constant)
                        (get-binding-info binding 'value))  ; no const folding
                   (function? obj))))                       ; no macro function
    ()))

(defun non-folded-local-binding? (binding)
  (if (binding? binding)
      (let ((obj (binding-obj? binding)))
        (null? (or (interface-binding? binding)  ; from interface file
                   (binding-imported? binding)    ; from just compiled module
                   (opencoding? obj)
                   (get-binding-info binding 'opencoding)
                   (get-binding-info binding 'ff)
                   (function? obj))))                       ; no macro function
    ()))

(defun get-binding-info (binding key)
  (get-binding-spec-info key (binding-info? binding)))

(defun get-binding-spec-info (key spec)
  (let ((res (assoc-list-ref spec key)))
    (if res (cdr res) ())))

(defgeneric binding-origin-module-name (binding))

(defmethod binding-origin-module-name ((binding <binding>))
  (module-name? (binding-module? binding)))

(defmethod binding-origin-module-name ((binding <interface-binding>))
  (binding-module? binding))

;;;-----------------------------------------------------------------------------
;;; Register delegated vars and binding-refs
;;;-----------------------------------------------------------------------------
(defun register-delegated-vars (vars)
  (lambda-delegated-vars!
   (dynamic *encl-lambda*)
   (append (lambda-delegated-vars? (dynamic *encl-lambda*)) vars)))

(defun register-binding-ref (binding)
  (let ((lambda (dynamic *encl-lambda*))
        (module (dynamic *actual-module*)))
    (if lambda
        (lambda-binding-refs!
         lambda (cons binding (lambda-binding-refs? lambda)))
      (module-lexical-binding-refs!
       module (cons binding (module-lexical-binding-refs? module))))))

(defmethod get-named-encl-lambda (fun)
  (and fun
       (if (eq (fun-name? fun) 'anonymous)
           (get-named-encl-lambda (syntax-expr-encl-lambda? fun))
         fun)))

;;;-----------------------------------------------------------------------------
;;; Clone syntax node
;;;-----------------------------------------------------------------------------
(defgeneric clone-node ((node <syntax-obj>)))

(defmethod clone-node ((node <binding>))
  (make (class-of node)
        local-name: (binding-local-name? node)
        module: (binding-module? node)
        immutable: (binding-immutable? node)
        imported: (binding-imported? node)
        obj: (binding-obj? node)
        local-index: (binding-local-index? node)
        info: (binding-info? node)))

;;;-----------------------------------------------------------------------------
)  ;; End of module sx-node
;;;-----------------------------------------------------------------------------
