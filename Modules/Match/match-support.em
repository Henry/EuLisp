;; match-scm.em -- scheme compatibility and support routines for match.em
(defmodule match-support
  (syntax ((except (match-let) macros) match0)
   import (level1)
   export (memq
           boolean?
           ;; How much of this is actually necessary?
           match:error match:andmap match:syntax-err match:set-error
           match:error-control match:disjoint-predicates
           match:vector-structures))

;; List-traversing
(defun memq (item collection) (member item collection eq))

;; Type predicates
(defun boolean? (obj) (or (eq obj t) (eq obj '())))

;; IO
(defconstant print-error (lambda (msg val) (error "~a ~s~%" msg val)))

;; Match support routines
(deflocal match:error
  (lambda (val . args)
    (do print args)
    (print-error "no matching clause for " val)))

(defun match:andmap (f l)
  (if (null? l)
      (and)
    (and (f (car l)) (match:andmap f (cdr l)))))

(defun match:syntax-err (obj msg)
  (print-error msg obj))

(defun match:set-error (v)
  (setq match:error v))

(deflocal match:error-control 'error)

(defun match:set-error-control (v)
  (setq match:error-control v))

(deflocal match:disjoint-predicates
  (cons 'null
        '(cons?
          symbol?
          boolean?
          number?
          string?
          character?
          function?
          vector?)))

(deflocal match:vector-structures '())

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
