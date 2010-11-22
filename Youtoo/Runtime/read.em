;;;-----------------------------------------------------------------------------
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
;;; Title: reader
;;; Library: level-1
;;;  Authors: Julian Padget, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule read
  (syntax (_macros
           read0)
   import (telos
           condition
           convert
           convert1
           vector
           string
           lock
           dynamic
           table
           stream
           format)
   export (<read-error>
           lispin
           read
           read-line
           read-char
           read-s-expression
           read-token
           sread
           sread-s-expression
           parse
           list-start
           list-stop
           dot
           vector-start
           vector-stop
           special-tokens
           quote-mark
           quasiquote-mark
           unquote-mark
           unquote-splicing-mark
           eos
           *dispatch-macro-character-table*
           set-dispatch-macro-character))

;;;-----------------------------------------------------------------------------
;;; Lispin
;;;-----------------------------------------------------------------------------
(defconstant lispin
  (make <stream>
        source: stdin
        read-action: (lambda (s eos-error? eos-value)
                       (parse (stream-source s) eos-error? eos-value))))

;;;-----------------------------------------------------------------------------
;;; Read syntax expression
;;;-----------------------------------------------------------------------------
(defun read options
  (match-let options
    ((stream lispin)
     (eos-error? t)
     (eos-value (eos-default-value)))
    (generic-read stream eos-error? eos-value)))

(defun read-s-expression options
  (match-let options
    ((stream stdin)
     (eos-error? t)
     (eos-value (eos-default-value)))
    (parse stream eos-error? eos-value)))

(defun parse (s eos-error? eos-value)
  (let parse-loop
    ((tok (ntok s special-tokens))
     (in-quasiquote ()))
    (cond
      ((special? tok)
       (cond
         ((eq tok list-start)
          (let list-loop ((so-far ()))
               (setq tok (ntok s special-tokens))
               (cond
                 ((eq tok list-stop)
                  (reverse-list so-far))
                 ((eq tok dot)
                  (let ((tail (ntok s special-tokens)))
                    (cond
                      ((eq tail unquote-mark)
                       (let ((tail2 (parse-loop (ntok s special-tokens)
                                                in-quasiquote)))
                         (if (and in-quasiquote
                                  (eq (ntok s special-tokens) list-stop))
                             (reverse-onto
                              so-far
                              (cons 'unquote
                                    (list (parse-loop tail2 ()))))
                           (read-error s "misplaced dot/unquote after ~s"
                                       (reverse-list so-far)))))
                      ((null? (eq (ntok s special-tokens) list-stop))
                       (read-error s "misplaced dot after ~s"
                                   (reverse-list so-far)))
                      (t (reverse-onto so-far tail)))))
                 ((eq tok eos)
                  (read-error s "unexpected end of file during list ~a"
                              ;; Avoid printing very long lists
                              (let ((l (reverse-list so-far)))
                                (if (fpi-binary< (list-size l) 128) l
                                  (list (car l) (car (cdr l))
                                        (car (car (cdr l))) " ... "
                                        (car (car (cdr so-far)))
                                        (car (cdr so-far)) (car so-far)))))
                  (reverse-list so-far))
                 (t (let ((tmp (parse-loop tok in-quasiquote)))
                      (list-loop (cons tmp so-far)))))))
         ((eq tok vector-start)
          (let vector-loop ((so-far ()))
               (setq tok (ntok s special-tokens))
               (cond
                 ((eq tok vector-stop)
                  (make-vector1 (list-size so-far) (reverse-list so-far)))
                 ((eq tok eos)
                  (read-error s "unexpected end of file during vector ~a"
                              (reverse-list so-far))
                  (make-vector1 (list-size so-far) (reverse-list so-far)))
                 (t (let ((tmp (parse-loop tok in-quasiquote)))
                      (vector-loop (cons tmp so-far)))))))
         ((eq tok quote-mark)
          (cons 'quote
                (list (parse-loop (ntok s special-tokens) in-quasiquote))))
         ((eq tok quasiquote-mark)
          (cons 'quasiquote
                (list (parse-loop (ntok s special-tokens) t))))
         ((eq tok unquote-mark)
          (if in-quasiquote
              (cons 'unquote
                    (list (parse-loop (ntok s special-tokens) ())))
            (progn
              (read-error s "misplaced unquote")
              (parse-loop (ntok s special-tokens) in-quasiquote))))
         ((eq tok unquote-splicing-mark)
          (if in-quasiquote
              (cons 'unquote-splicing
                    (list (parse-loop (ntok s special-tokens) ())))
            (progn
              (read-error s "misplaced unquote-splicing")
              (parse-loop (ntok s special-tokens) in-quasiquote))))
         ((eq tok object-comment) ; Scheme-style #; object comment
          (parse-loop (ntok s special-tokens) ()) ; ignore object
          (parse-loop (ntok s special-tokens) ())) ; return next object
         ((eq tok eos)
          (if eos-error? (end-of-stream s) eos-value))
         (t (read-error s "unexpected token ~a" (tag tok))
            ())))
      (t
       (let ((fun (table-ref *dispatch-macro-character-table* tok)))
         (if (function? fun)
             (fun s tok ())
           tok))))))

(defun reverse-onto (a b)
  (if (null? a) b
    (reverse-onto (cdr a) (cons (car a) b))))

;;;-----------------------------------------------------------------------------
;;; Macro dispatch characters
;;;-----------------------------------------------------------------------------
(deflocal *dispatch-macro-character-table* (make <table>))

(defun set-dispatch-macro-character (char1 char2 fun)
  (let ((mdcharsym
         (make <symbol> name: (convert (list char1 char2) <string>))))
    ((setter table-ref)
     *dispatch-macro-character-table* mdcharsym fun)))

;;;-----------------------------------------------------------------------------
;;; Read character
;;;-----------------------------------------------------------------------------
(defun read-char options
  (match-let options
    ((stream stdin)
     (eos-error? t)
     (eos-value (eos-default-value)))
    (generic-read stream eos-error? eos-value)))

;;;-----------------------------------------------------------------------------
;;; Read next line string
;;;-----------------------------------------------------------------------------
(defun read-line options
  ;; cheap and cheerful version
  (match-let options
    ((stream stdin)
     (eos-error? t)
     (eos-value (eos-default-value)))
    (let loop ((c (generic-read stream eos-error? eos-value))
               (result ()))
         (cond
           ((eql c #\\n)
            (convert (reverse-list (cons c result)) <string>))
           ((eq c eos-value)
            (if (null? result)
                eos-value
              (convert (reverse-list result) <string>)))
           (t
            (loop (generic-read stream eos-error? eos-value)
                  (cons c result)))))))

;;;-----------------------------------------------------------------------------
;;; Read token
;;;-----------------------------------------------------------------------------
(defun read-token options
  (match-let options
    ((stream stdin)
     (eos-error? t)
     (eos-value (eos-default-value)))
    (let ((tok (ntok stream special-tokens)))
      (if (eq tok eos)
          (if eos-error? (end-of-stream stream) eos-value)
        tok))))

;;;-----------------------------------------------------------------------------
;;; Read into string
;;;-----------------------------------------------------------------------------
(deflocal *sread-string-stream* (make <string-stream> string: ""))

(defun sread (string . options)
  (match-let options
    ((eos-error? t)
     (eos-value ()))
    (with-lock (stream-lock *sread-string-stream*)
               (initialize (stream-source *sread-string-stream*)
                           (list buffer: string))
               (read *sread-string-stream*
                     eos-error? eos-value))))

(defun sread-s-expression (string . options)
  (match-let options
    ((eos-error? t)
     (eos-value ()))
    (with-lock (stream-lock *sread-string-stream*)
               (initialize (stream-source *sread-string-stream*)
                           (list buffer: string))
               (parse *sread-string-stream*
                      eos-error? eos-value))))

;;;-----------------------------------------------------------------------------
;;; Read error
;;;-----------------------------------------------------------------------------
(defcondition <read-error> <stream-condition> ())

(defun read-error (s msg . args)
  (error <read-error>
         (apply fmt
                (string-append msg " at characters ~d")
                (append args
                        (list (control-block-stream-pos (stream-source s)))))
         value: s))

;;;-----------------------------------------------------------------------------
;;; Special tags
;;;-----------------------------------------------------------------------------
(defclass <special> ()
  ((tag keyword: tag: accessor: tag))
  predicate: special?)

(defconstant list-start (make <special> tag: "(" ))
(defconstant list-stop  (make <special> tag: ")" ))
(defconstant vector-start (make <special> tag: "#(" ))
(defconstant vector-stop list-stop)
(defconstant quote-mark (make <special> tag: "'" ))
(defconstant quasiquote-mark (make <special> tag: "`" ))
(defconstant unquote-mark (make <special> tag: "," ))
(defconstant unquote-splicing-mark (make <special> tag: ",@" ))
(defconstant dot (make <special> tag: "." ))
(defconstant object-comment (make <special> tag: "#;" ))
(defconstant eos (make <special> tag: "<end of stream>" ))

(defconstant special-tokens
  (make-vector 11
               list-start
               list-stop
               vector-start
               vector-stop
               quote-mark
               quasiquote-mark
               unquote-mark
               unquote-splicing-mark
               dot
               object-comment
               eos))

;;;-----------------------------------------------------------------------------
)  ;; End of module read
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Test
;;;-----------------------------------------------------------------------------
(let/cc exit
  (let loop ((x ()))
       (print "? " nl)
       (let/cc restart
         (with-handler
          (lambda (c k)
            (output-condition-contents c)
            (restart ()))
          (setq x (read lispin () (eos-default-value)))
          (if (eq x (eos-default-value))
              (progn (print "Exiting" nl) (exit 0))
            (format "- ~s\n" x))))
       (loop x)))
