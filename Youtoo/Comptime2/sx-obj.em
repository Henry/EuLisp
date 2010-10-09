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
;;; Title: defining all classes of the abstract syntax tree
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses)
;;;  Authors: Andreas Kind, Keith Playford
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule sx-obj
  (syntax (_macros
           _sx-obj0)
   import (i-level1
           sx-obj1)
   expose (sx-obj1
           sx-obj2)
   ;; generated accessors and predicates are automatiacally exported
   export (local-name?
           origin-name?
           ;binding?
           binding!
           save-binding-local-name?
           save-binding-module-name?))

;;;-----------------------------------------------------------------------------
;;;  Functional nodes
;;;-----------------------------------------------------------------------------
(def-syntax-obj <fun> (<syntax-expr>)
                (name
                 binding
                 args
                 arity
                 range-and-domain
                 body
                 appls
                 has-unknown-appls))

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
  (if (binding? obj)
      (binding-local-name? obj)
    (let ((x (slot-value obj 'binding)))
      (and x (binding-local-name? x)))))

(defun save-binding-local-name? (x)
  ;; Sometimes x is a hard-code level1 binding (i.e. a list)
  (if (cons? x)
      (cdr (cdr x))
    (if (binding? x)
        (binding-local-name? x)
      x)))

(defun save-binding-module-name? (x)
  ;; Sometimes x is a hard-code level1 binding (i.e. a list)
  (if (cons? x)
      (car (cdr x))
    (if (binding? x)
        (let ((m (binding-module? x)))
          (if (module? m) (module-name? m) m))
      (module-name? (dynamic *actual-module*)))))

;(defun binding? (obj) (slot-value obj 'binding))
;;(declare-inline binding?)

(defun binding! (obj value)
  (let* ((slots (class-slots (class-of obj)))
         (slot (member-list 'binding slots
                            (lambda (name descr)
                              (eq name (slot-name descr)))))
         (writer (and slot (slot-writer (car slot)))))
    (writer obj value)))

;;;-----------------------------------------------------------------------------
)  ;; End of module sx-obj
;;;-----------------------------------------------------------------------------
