;; match-scm.em -- scheme compatibility and support routines for match.em
(defmodule match-support
  (syntax ((except (match-let) macros) match0)
   import (level1)
   export (caaadr caadar cadaar cadadr caddar cdaadr cdadar cddaar cddadr
           cdddar cddddr
           assoc assq memq
           boolean? else
           ;; How much of this is actually necessary?
           match:error match:andmap match:syntax-err match:set-error
           match:error-control match:disjoint-predicates
           match:vector-structures))

  ;; Getters
  (defun caaadr (l) (car (car (car (cdr l)))))

  (defun caadar (l) (car (car (cdr (car l)))))
  (defun cadaar (l) (car (cdr (car (car l)))))

  (defun cadadr (l) (car (cdr (car (cdr l)))))
  (defun caddar (l) (car (cdr (cdr (car l)))))

  (defun cdaadr (l) (cdr (car (car (cdr l)))))
  (defun cdadar (l) (cdr (car (cdr (car l)))))

  (defun cddaar (l) (cdr (cdr (car (car l)))))
  (defun cddadr (l) (cdr (cdr (car (cdr l)))))

  (defun cdddar (l) (cdr (cdr (cdr (car l)))))
  (defun cddddr (l) (cdr (cdr (cdr (cdr l)))))

  ;; List-traversing predicates
  (defgeneric assoc  (item obj . test))

  (defmethod assoc (item (l <list>) . test)
    (let ((test (if (null? test) binary= (car test))))
      (let loop ((l l))
           (if (null? l)
               '()
             (if (test (caar l) item)
                 (car l)
               (loop (cdr l)))))))

  (defun assq (item l) (assoc item l eq))
  (defun memq (item collection) (member item collection eq))

  ;; Type predicates
  (defun boolean? (obj) (or (eq obj t) (eq obj '())))

  ;; IO
  (defconstant print-error (lambda (msg val) (error "~a ~s~%" msg val)))

  ;; Constants
  (defconstant else t)

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
