;; match-scm.em -- scheme compatibility and support routines for match.em
(defmodule match-support
  (syntax (macros match0)
   import (level1)
   export (caaadr caadar cadaar cadadr caddar cdaadr cdadar cddaar cddadr
           cdddar cddddr
           vector-ref
           set-car! set-cdr! vector-set!
           length string-length vector-length
           assoc assq memq
           vector gentemp
           equal? eq?
           boolean? char? list? pair? string? symbol? vector?
           list->vector number->string string->list string->number
           string->symbol symbol->string vector->list
           char-numeric?
           for-each
           display pretty-print slib:error
           else
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

  (defun vector-ref (obj index) (element obj index))

  ;; Setters
  (defun set-car! (place item) ((setter car) place item))

  (defun set-cdr! (place item) ((setter cdr) place item))
  (defun vector-set! (obj index val) (element obj index val))

  ;; Size
  (defconstant length size)
  (defconstant string-length string-size)
  (defconstant vector-length vector-size)

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

  ;; Constructors
  (defun vector l (convert l <vector>))
  (defconstant gentemp gensym)

  ;; Equality and Equvialence
  (defconstant equal? binary=)
  (defconstant eq? eq)

  ;; Type predicates
  (defun boolean? (obj) (or (eq obj t) (eq obj '())))
  (defconstant char? character?)
  (defconstant list? list?)
  (defconstant pair? consp)
  (defconstant string? string?)
  (defconstant symbol? symbol?)
  (defconstant vector? vector?)

  ;; Conversions
  (defun list->vector (l) (convert l <vector>))

  (defun number->string (num) (convert num <string>))
  (defun string->list (str) (convert str <list>))

  (defun string->number (str) (convert str <int>))
  (defun string->symbol (str) (convert str <symbol>))

  (defun symbol->string (sym) (convert sym <string>))
  (defun vector->list (v) (convert v <list>))

  ;; Character predicates
  (defconstant char-numeric? digit?)

  ;; Control structure
  (defconstant for-each do)

  ;; IO
  (defconstant display prin)
  (defconstant pretty-print print)
  (defconstant slib:error (lambda (msg val) (error "~a ~s~%" msg val)))

  ;; Constants
  (defconstant else t)

  ;; Match support routines
  (define match:error
    (lambda (val . args)
      (for-each pretty-print args)
      (slib:error "no matching clause for " val)))

  (define match:andmap
    (lambda (f l)
      (if (null? l)
          (and)
        (and (f (car l)) (match:andmap f (cdr l))))))

  (define match:syntax-err
    (lambda (obj msg) (slib:error msg obj)))

  (define match:set-error (lambda (v) (set! match:error v)))

  (define match:error-control 'error)

  (define match:set-error-control
    (lambda (v) (set! match:error-control v)))

  (define match:disjoint-predicates
    (cons 'null
          '(pair?
            symbol?
            boolean?
            number?
            string?
            char?
            procedure?
            vector?)))

  (define match:vector-structures '())

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
