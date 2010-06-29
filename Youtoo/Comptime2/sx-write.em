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
        (spprint stream module)
        (sformat stream "\nPretty printed environment of module ~a:"
                (module-name? module))
        (sformat stream "\n  lexical-env:")
        (access-table-do
         (lambda (name binding)
           ;; Attention -- name is ptr to C string!
           (sformat stream "\n   (~a . ~a)"
                   (binding-local-name? binding)
                   (binding-prin-string binding)))
         (module-lexical-env? module))
        (sformat stream "\n  external-env:")
        (access-table-do
         (lambda (name binding)
           ;; Attention -- name is ptr to C string!
           (sformat stream "\n   (~a . ~a)"
                   (binding-local-name? binding)
                   (binding-prin-string binding)))
         (module-external-env? module))
        (sformat stream "\n  syntax-env:")
        (access-table-do
         (lambda (name binding)
           ;; Attention -- name is ptr to C string!
           (sformat stream "\n   (~a . ~a)"
                   (binding-local-name? binding)
                   (binding-prin-string binding)))
         (module-syntax-env? module))
        (snewline stream))
      module))

  (defun binding-prin-string (binding)
    (let ((obj (binding-obj? binding)))
      (if (function? obj)
          "#<macro-function>"
        (fmt "~a" obj))))

  (defmethod generic-prin ((x <module>) (s <stream>))
    (sformat s "#<module: ~a>" (module-name? x)))

  (defmethod generic-prin ((x <binding>) (s <stream>))
    (let* ((module (binding-module? x))
           (local-name (binding-local-name? x))
           (local-index (binding-local-index? x))
           (module-name (if (module? module) (module-name? module) module)))
      (if (int? local-index)
          (sformat s "#<binding: ~a:~a:~a>" module-name local-name local-index)
        (sformat s "#<binding: ~a:~a>" module-name local-name))))

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
    (sformat s "#<macro-function>"))

  (defmethod new-generic-prin ((x <syntax-obj>) s)
    (if (member1-list 'binding (find-slot-names x))
        (sformat s "~a" (slot-value x 'binding))
      (sformat s "<unprintable syntax object>")))

  (defmethod new-generic-prin ((x <module>) s)
    (sformat s "~a" (module-name? x)))

  (defmethod new-generic-prin ((x <binding>) s)
    (sformat s "~a" (binding-local-name? x)))

  (defmethod new-generic-prin ((x <var>) s)
    (sformat s "~a" (var-name? x)))

  (defmethod new-generic-prin ((x <setq>) s)
    (sformat s "(setq ~a ~a)" (setq-binding? x) (setq-obj? x)))

  (defmethod new-generic-prin ((x <named-const>) s)
    (sformat s "~a" (named-const-name? x)))

  (defmethod new-generic-prin ((x <literal-const>) s)
    (sformat s "~a" (const-value? x)))

  ;  (defmethod new-generic-prin ((x <keywrd>) s)
  ;    (sformat s "~a" (const-value? x)))

  (defmethod new-generic-prin ((x <lambda>) s)
    (if (and (number? (dynamic *pprint*)) (< (dynamic *pprint*) 2))
        (sformat s "~a" (fun-name? x))
      (sformat s "(~a ~a ~a)"
              (if (lambda-inlined? x)
                  "inlined-lambda"
                "lambda")
              (fun-args? x) (fun-body? x))))

  (defmethod new-generic-prin ((x <opencoding>) s)
    (sformat s "(opencoded-lambda ~a ~a)" (fun-args? x) (fun-body? x)))

  (defmethod new-generic-prin ((x <let*>) s)
    (sformat s "(let* (")
    (do1-list (lambda (var)
                (sformat s "(~a ~a)" var (and (var? var) (var-value? var))))
              (fun-args? x))
    (sformat s ") ~a)" (fun-body? x)))

  (defmethod new-generic-prin ((x <appl>) s)
    (sformat s "~a" (cons (appl-fun? x) (appl-args? x))))

  ;  (defmethod new-generic-prin ((x <progn>) s)
  ;    (sformat s "(progn ~a)" (progn-forms? x)))

  (defmethod new-generic-prin ((x <if>) s)
    (sformat s "(if ~a ~a ~a)" (if-pred? x) (if-then? x) (if-else? x)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
