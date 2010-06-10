;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: defining all classes of the abstract syntax tree
;;;-----------------------------------------------------------------------------
(defmodule sx-obj
  (syntax (_macros _sx-obj0)
   import (i-level1 sx-obj1)
   expose (sx-obj1 sx-obj2)
  ;; generated accessors and predicates are automatiacally exported
   export (local-name? origin-name? binding? binding!
           save-binding-local-name? save-binding-module-name?))

;;;-----------------------------------------------------------------------------
;;;  Functional nodes
;;;-----------------------------------------------------------------------------
  (def-syntax-obj <fun> (<syntax-expr>)
    (name binding args arity range-and-domain body appls has-unknown-appls))

  (def-syntax-obj <lambda> (<fun>) (inlined delegated-vars binding-refs))
  ;;(def-syntax-obj <init-lambda> (<lambda>) (pre-body module))
  (def-syntax-obj <opencoding> (<fun>) ())
  (def-syntax-obj <let*> (<fun>) ())
  ;;(def-syntax-obj <labels> (<syntax-expr>) (funs body))
  (def-syntax-obj <appl> (<syntax-expr>) (fun args))
  (def-syntax-obj <call-next-method> (<appl>) ())

;;;-----------------------------------------------------------------------------
;;;  Control structures
;;;-----------------------------------------------------------------------------
  ;;(def-syntax-obj <progn> (<syntax-obj>) (forms))
  (def-syntax-obj <if> (<syntax-obj>) (pred then else))
  ;;(def-syntax-obj <let-cc> (<syntax-obj>) (body cont))
  ;;(def-syntax-obj <loop> (<syntax-obj>) (pred forms))
  ;;(def-syntax-obj <continue> (<syntax-obj>) (loop inits))
  ;;(def-syntax-obj <loop-exit> (<syntax-obj>) (loop))
  ;;(def-syntax-obj <return> (<syntax-expr>) (form))
  ;;(def-syntax-obj <cont> (<syntax-def>) ())

;;;-----------------------------------------------------------------------------
;;;  Import, export
;;;-----------------------------------------------------------------------------
  ;;(def-syntax-obj <import> (<syntax-obj>) (import-specs))
  ;;(def-syntax-obj <import-spec> (<syntax-obj>) (module-name))
  ;;(def-syntax-obj <simple-import> (<import-spec>) ())
  ;;(def-syntax-obj <filtered-import> (<import-spec>) (bindings))
  ;;(def-syntax-obj <rename-import> (<filtered-import>) ())
  ;;(def-syntax-obj <except-import> (<filtered-import>) ())
  ;;(def-syntax-obj <only-import> (<filtered-import>) ())
  ;;(def-syntax-obj <expose> (<syntax-obj>) (import-specs))

;;;-----------------------------------------------------------------------------
;;;  Aux functions to prevent multiple inheritance with syntax objects
;;;-----------------------------------------------------------------------------
  (defun origin-name? (obj) (slot-value obj 'name))
  ;(declare-inline origin-name?)

  (defun local-name? (obj)
    (if (bindingp obj)
        (binding-local-name? obj)
      (let ((x (slot-value obj 'binding)))
        (and x (binding-local-name? x)))))

  (defun save-binding-local-name? (x)
    ;; Sometimes x is a hard-code level1 binding (i.e. a list)
    (if (consp x)
        (cdr (cdr x))
      (if (bindingp x)
          (binding-local-name? x)
        x)))

  (defun save-binding-module-name? (x)
    ;; Sometimes x is a hard-code level1 binding (i.e. a list)
    (if (consp x)
        (car (cdr x))
      (if (bindingp x)
          (let ((m (binding-module? x)))
            (if (modulep m) (module-name? m) m))
        (module-name? (dynamic *actual-module*)))))

  (defun binding? (obj) (slot-value obj 'binding))
  ;(declare-inline binding?)

  (defun binding! (obj value)
    (let* ((slots (class-slots (class-of obj)))
           (slot (member-list 'binding slots
                              (lambda (name descr)
                                (eq name (slot-name descr)))))
           (writer (and slot (slot-writer (car slot)))))
      (writer obj value)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
