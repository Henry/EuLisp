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
;;; Title: print support for abstract syntax tree
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule sx-write
  (syntax (_syntax-1)
   import (i-all
           sx-obj)
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
                             (binding-print-string binding)))
                  (module-lexical-env? module))
                 (sformat stream "\n  external-env:")
                 (access-table-do
                  (lambda (name binding)
                    ;; Attention -- name is ptr to C string!
                    (sformat stream "\n   (~a . ~a)"
                             (binding-local-name? binding)
                             (binding-print-string binding)))
                  (module-external-env? module))
                 (sformat stream "\n  syntax-env:")
                 (access-table-do
                  (lambda (name binding)
                    ;; Attention -- name is ptr to C string!
                    (sformat stream "\n   (~a . ~a)"
                             (binding-local-name? binding)
                             (binding-print-string binding)))
                  (module-syntax-env? module))
                 (sprint stream nl))
    module))

(defun binding-print-string (binding)
  (let ((obj (binding-obj? binding)))
    (if (function? obj)
        "#<syntax-operator>"
      (fmt "~a" obj))))

(defmethod generic-print ((x <module>) (s <stream>))
  (sformat s "#<module: ~a>" (module-name? x)))

(defmethod generic-print ((x <binding>) (s <stream>))
  (let* ((module (binding-module? x))
         (local-name (binding-local-name? x))
         (local-index (binding-local-index? x))
         (module-name (if (module? module) (module-name? module) module)))
    (if (fpi? local-index)
        (sformat s "#<binding: ~a:~a:~a>" module-name local-name local-index)
      (sformat s "#<binding: ~a:~a>" module-name local-name))))

;;;-----------------------------------------------------------------------------
;;; New generic printing (depends on *pprint*)

;;;-----------------------------------------------------------------------------
(defmethod generic-print ((x <syntax-obj>) (s <stream>))
  (if (dynamic *pprint*)
      (new-generic-print x s)
    (call-next-method)))

;;;-----------------------------------------------------------------------------
;;; Generic printing of the abstract syntax objects
;;;-----------------------------------------------------------------------------
(defgeneric new-generic-print (x s))

(defmethod new-generic-print ((x <function>) s)
  (sformat s "#<syntax-operator>"))

(defmethod new-generic-print ((x <syntax-obj>) s)
  (if (member1-list 'binding (find-slot-names x))
      (sformat s "~a" (slot-value x 'binding))
    (sformat s "<unprintable syntax object>")))

(defmethod new-generic-print ((x <module>) s)
  (sformat s "~a" (module-name? x)))

(defmethod new-generic-print ((x <binding>) s)
  (sformat s "~a" (binding-local-name? x)))

(defmethod new-generic-print ((x <var>) s)
  (sformat s "~a" (var-name? x)))

(defmethod new-generic-print ((x <setq>) s)
  (sformat s "(setq ~a ~a)" (setq-binding? x) (setq-obj? x)))

(defmethod new-generic-print ((x <named-const>) s)
  (sformat s "~a" (named-const-name? x)))

(defmethod new-generic-print ((x <literal-const>) s)
  (sformat s "~a" (const-value? x)))

;  (defmethod new-generic-print ((x <keywrd>) s)
;    (sformat s "~a" (const-value? x)))

(defmethod new-generic-print ((x <lambda>) s)
  (if (and (number? (dynamic *pprint*)) (< (dynamic *pprint*) 2))
      (sformat s "~a" (fun-name? x))
    (sformat s "(~a ~a ~a)"
             (if (lambda-inlined? x)
                 "inlined-lambda"
               "lambda")
             (fun-args? x) (fun-body? x))))

(defmethod new-generic-print ((x <opencoding>) s)
  (sformat s "(opencoded-lambda ~a ~a)" (fun-args? x) (fun-body? x)))

(defmethod new-generic-print ((x <let*>) s)
  (sformat s "(let* (")
  (do1-list (lambda (var)
              (sformat s "(~a ~a)" var (and (var? var) (var-value? var))))
            (fun-args? x))
  (sformat s ") ~a)" (fun-body? x)))

(defmethod new-generic-print ((x <appl>) s)
  (sformat s "~a" (cons (appl-fun? x) (appl-args? x))))

;  (defmethod new-generic-print ((x <progn>) s)
;    (sformat s "(progn ~a)" (progn-forms? x)))

(defmethod new-generic-print ((x <if>) s)
  (sformat s "(if ~a ~a ~a)" (if-pred? x) (if-then? x) (if-else? x)))

;;;-----------------------------------------------------------------------------
)  ;; End of module sx-write
;;;-----------------------------------------------------------------------------
