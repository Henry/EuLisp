;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: print support for abstract syntax tree
;;;-----------------------------------------------------------------------------
(defmodule sx-write
  (syntax (_macros)
   import (i-all sx-obj)
   export (pprint-module))

;;;-----------------------------------------------------------------------------
;;; Print a module
;;;-----------------------------------------------------------------------------
  (defun pprint-module (module . streams)
    (let ((stream (or (and streams (car streams)) t)))
      (dynamic-let ((*pprint* t))
        (pprint module stream)
        (format stream "\nPretty printed environment of module ~a:"
                (module-name? module))
        (format stream "\n  lexical-env:")
        (access-table-do
         (lambda (name binding)
           ;; Attention -- name is ptr to C string!
           (format stream "\n   (~a . ~a)"
                   (binding-local-name? binding)
                   (binding-prin-string binding)))
         (module-lexical-env? module))
        (format stream "\n  external-env:")
        (access-table-do
         (lambda (name binding)
           ;; Attention -- name is ptr to C string!
           (format stream "\n   (~a . ~a)"
                   (binding-local-name? binding)
                   (binding-prin-string binding)))
         (module-external-env? module))
        (format stream "\n  syntax-env:")
        (access-table-do
         (lambda (name binding)
           ;; Attention -- name is ptr to C string!
           (format stream "\n   (~a . ~a)"
                   (binding-local-name? binding)
                   (binding-prin-string binding)))
         (module-syntax-env? module))
        (newline stream))
      module))

  (defun binding-prin-string (binding)
    (let ((obj (binding-obj? binding)))
      (if (function? obj)
          "#<macro-function>"
        (format () "~a" obj))))

  (defmethod generic-prin ((x <module>) (s <stream>))
    (format s "#<module: ~a>" (module-name? x)))

  (defmethod generic-prin ((x <binding>) (s <stream>))
    (let* ((module (binding-module? x))
           (local-name (binding-local-name? x))
           (local-index (binding-local-index? x))
           (module-name (if (modulep module) (module-name? module) module)))
      (if (int? local-index)
          (format s "#<binding: ~a:~a:~a>" module-name local-name local-index)
        (format s "#<binding: ~a:~a>" module-name local-name))))

;;;-----------------------------------------------------------------------------
;;; New generic printing (depends on *pprint*)

;;;-----------------------------------------------------------------------------
  (defmethod generic-prin ((x <syntax-obj>) (s <stream>))
    (if (dynamic *pprint*)
        (new-generic-prin x s)
      (call-next-method)))

;;;-----------------------------------------------------------------------------
;;; Generic printing of the abstract syntax objects
;;;-----------------------------------------------------------------------------
  (defgeneric new-generic-prin (x s))

  (defmethod new-generic-prin ((x <function>) s)
    (format s "#<macro-function>"))

  (defmethod new-generic-prin ((x <syntax-obj>) s)
    (if (member1-list 'binding (find-slot-names x))
        (format s "~a" (slot-value x 'binding))
      (format s "<unprintable syntax object>")))

  (defmethod new-generic-prin ((x <module>) s)
    (format s "~a" (module-name? x)))

  (defmethod new-generic-prin ((x <binding>) s)
    (format s "~a" (binding-local-name? x)))

  (defmethod new-generic-prin ((x <var>) s)
    (format s "~a" (var-name? x)))

  (defmethod new-generic-prin ((x <setq>) s)
    (format s "(setq ~a ~a)" (setq-binding? x) (setq-obj? x)))

  (defmethod new-generic-prin ((x <named-const>) s)
    (format s "~a" (named-const-name? x)))

  (defmethod new-generic-prin ((x <literal-const>) s)
    (format s "~a" (const-value? x)))

  ;  (defmethod new-generic-prin ((x <keywrd>) s)
  ;    (format s "~a" (const-value? x)))

  (defmethod new-generic-prin ((x <lambda>) s)
    (if (and (number? (dynamic *pprint*)) (< (dynamic *pprint*) 2))
        (format s "~a" (fun-name? x))
      (format s "(~a ~a ~a)"
              (if (lambda-inlined? x)
                  "inlined-lambda"
                "lambda")
              (fun-args? x) (fun-body? x))))

  (defmethod new-generic-prin ((x <opencoding>) s)
    (format s "(opencoded-lambda ~a ~a)" (fun-args? x) (fun-body? x)))

  (defmethod new-generic-prin ((x <let*>) s)
    (format s "(let* (")
    (do1-list (lambda (var)
                (format s "(~a ~a)" var (and (varp var) (var-value? var))))
              (fun-args? x))
    (format s ") ~a)" (fun-body? x)))

  (defmethod new-generic-prin ((x <appl>) s)
    (format s "~a" (cons (appl-fun? x) (appl-args? x))))

  ;  (defmethod new-generic-prin ((x <progn>) s)
  ;    (format s "(progn ~a)" (progn-forms? x)))

  (defmethod new-generic-prin ((x <if>) s)
    (format s "(if ~a ~a ~a)" (if-pred? x) (if-then? x) (if-else? x)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
