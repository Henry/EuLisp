;;;-----------------------------------------------------------------------------
;;;                 OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;; File   : reader.em
;;; Date   :  6 Jul 1995
;;; Author : Tracy Gardner (tag@maths.bath.ac.uk)
;;; Description: Code to read in an OPS5 program from file and create
;;; corresponding objects.
;;;-----------------------------------------------------------------------------
(defmodule reader
  (syntax (syntax-0 macros-tag)
   import (level-0 basic reader-ce reader-vars
                  prod-gf prod cond-el-gf cond-el-1
                  reader-act ops-out)) ;; ops-reader))

(defconstant <input-port> <file-stream>)

(defun read-next (in)
  (read-s-expression in () (eos-default-value)))

(defun end-of-input (next)
  (eql next (eos-default-value)))

(defun open-ops-file (name)
  (make <file-stream> file-name: name mode: 'r))

(print "### reader.em" nl)

(defconstant ops-err format)
(defconstant ops-warn format)

;;;-----------------------------------------------------------------------------
;;; read-ops-prog
; Reads in an OPS5 program from file.
;;;-----------------------------------------------------------------------------
(defun read-ops-prog (filename)
  (format "Trying to open file: ~a~%" filename)
  (let ((reader (make <reader>)))
    ((setter ops-in) reader (open-ops-file filename))
    (sformat ops-out "Loading file ~a ...~%" filename)
    (read-prog reader)
    (disconnect (ops-in reader))
    reader))

(defun ops-read (s)
  (let ((next (read-next s)))
    (sformat ops-out "Next: ~a~%" next)
    next))

;;;-----------------------------------------------------------------------------
;;; class: reader
; Class to read in an OPS5 program
;;;-----------------------------------------------------------------------------
(defclass <reader> ()
  ((input
    keyword: input:
    accessor:  ops-in)
   (in-prods
    default:  NIL
    accessor: in-prods)
   (current-prod
    default: ()
    accessor: current-prod)
   (prods
    default: ()
    reader:  prods
    writer:  set-prods)
   (ces
    default: ()
    accessor: ces)
   (ce-man
    default: (make-ce-manager)
    reader: ce-man
    writer: set-ce-man)
   (lits
    default: (make <table>)
    reader:  lits
    writer:  set-lits)))

(defun read-prog (reader)
  ;;     (sformat ops-out "read-prog:...~a ~%" reader)
  (let ((next (ops-read (ops-in reader))))
    (cond
      ((end-of-input next) t)
      ((eql (car next) 'literalize)
       (read-prog (read-lit reader (cdr next))))
      ((eql (car next) 'p) ; found a production
       (read-prog (read-prod reader (cdr next))))
      (t (ops-err "Unknown OPS5 structure: ~a~%: " next)))))

;;;-----------------------------------------------------------------------------
;;; read-lit
; Process a literalize declaration
;;;-----------------------------------------------------------------------------
(defun read-lit (reader stment)
  ;;     (sformat ops-out "read-lit: ~%")
  ;; OPS5 does not allow literalize statements after the first
  ;; production.
  (when (in-prods reader)
        (ops-warn "Literalize statement after start of productions"))
  (add-class reader (car stment))
  (let ((class-name (car stment)))
    (labels ((loop (attribs index)
                   (cond
                     ((null? attribs) reader)
                     (t
                      (add-attrib reader class-name
                                  (car attribs) index)
                      (loop (cdr attribs) (+ index 1))))))
            (loop (cdr stment) 0))))

;;;-----------------------------------------------------------------------------
;;; add-class
; Adds a table to store attribute names and indices of a WME class
; declared in a literalize statement
;;;-----------------------------------------------------------------------------
;;;
(defun add-class (reader class-name)
  ((setter element) (lits reader) class-name (make <table>)))

;;;-----------------------------------------------------------------------------
;;; add-attrib
; Adds an entry to the class-name table from attrib to index
;;;-----------------------------------------------------------------------------
(defun add-attrib (reader class-name attrib index)
  ;;     (sformat ops-out "add-attrib: ~a~%" (make-attrib attrib))
  ((setter element) (element (lits reader) class-name)
   (make-attrib attrib) index)
  reader)

;;;-----------------------------------------------------------------------------
;;; read-prod
; Process a production
;;;-----------------------------------------------------------------------------
(defun read-prod (reader new-prod-in)
  ;;(sformat ops-out "New production: ~a~%" (car new-prod-in))
  ;; pass over production sending items to read-ce and
  ;; read-action as appropriate.
  (let* ((prod-name (car new-prod-in))
         (new-prod1 (cdr new-prod-in))
         (res (get-join-vars new-prod1))
         (join-vars (car res))
         (new-prod  (cdr res))
         (curr-prod (make-production prod-name)))
    ;;(format "Prod: ~a~%" new-prod)
    (set-prods reader (cons curr-prod (prods reader)))
    (labels ((get-ce (ce-rest is-neg)
                     (let ((attrib-table (lits reader)))
                       (cond
                         ((eql (car ce-rest) '-->)
                          (get-action (cdr ce-rest)))
                         ((eql (car ce-rest) '-)
                          (get-ce (cdr ce-rest) t))
                         ((eql (car ce-rest) '{)
                               (if (atom? (cadr ce-rest))
                                   (progn
                                     ;;(format "Found: ~a~%" (cadr ce-rest))
                                     (let* ((cl (car (caddr ce-rest)))
                                            (res-ce
                                             (insert-new-ce (ce-man reader)
                                                            (read-ce is-neg
                                                                     (element
                                                                      attrib-table
                                                                      cl)
                                                                     (caddr ce-rest)
                                                                     join-vars)
                                                            curr-prod)))
                                       (add-cond-el curr-prod res-ce)
                                       (set-prod-ce-vars
                                        curr-prod
                                        (cons (cons (cadr ce-rest)
                                                    res-ce)
                                              (prod-ce-vars curr-prod)))))
                                 (progn
                                   (let* ((cl (make-attrib (car (cadr ce-rest))))
                                          (res-ce (insert-new-ce
                                                   (ce-man reader)
                                                   (read-ce is-neg
                                                            (element
                                                             attrib-table cl)
                                                            (cadr ce-rest)
                                                            join-vars)
                                                   curr-prod)))
                                     ;;(format "Found: ~a~%" (caddr ce-rest))
                                     (add-cond-el curr-prod res-ce)
                                     (set-prod-ce-vars
                                      curr-prod
                                      (cons (cons (caddr ce-rest)
                                                  res-ce)
                                            (prod-ce-vars curr-prod))))))
                               (get-ce (cddddr ce-rest) ()))
                          ((null? (cdr ce-rest)) ;; no tests
                           (let ((res (insert-new-ce
                                       (ce-man reader)
                                       (make-pos-njoin-ce (car ce-rest))
                                       curr-prod)))
                             ;;(format "curr-prod: ~a res: ~a~%" curr-prod res)
                             (add-cond-el curr-prod res)))
                          (t
                           (let ((cl (make-attrib (caar ce-rest))))
                             (let ((res (insert-new-ce
                                         (ce-man reader)
                                         (read-ce is-neg
                                                  (element attrib-table cl)
                                                  (car ce-rest) join-vars)
                                         curr-prod)))
                               ;;(format "curr-prod: ~a res: ~a~%" curr-prod res)
                               (add-cond-el curr-prod res))
                             (get-ce (cdr ce-rest) ()))))))
                     (get-action (prod)
                                 (cond
                                   ((null? prod))
                                   (t
                                    (read-action reader (car prod) curr-prod)
                                    (get-action (cdr prod))))))
             (get-ce new-prod ())))
    reader)

;;;-----------------------------------------------------------------------------
;;; check-for-ce-var
  ; Checks to see if a variable is attached to the next condition element
  ; Returns ()      -- no ce var
  ;         'before -- ce var appears before ce
  ;         'after  -- ce var appears after ce
;;;-----------------------------------------------------------------------------
  (defun check-for-ce-var (prod)
    (unless (eql (car prod) '{) ())
            (if (atom? (cadr prod))
                'before
              'after))

    (export read-ops-prog ce-man <reader>)

;;;-----------------------------------------------------------------------------
    )  ;; End of module
;;;-----------------------------------------------------------------------------
