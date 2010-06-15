;;; format.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule format
    (import (root condcl thread setter convert)
     export (format))

  (deflocal escape-char #\~)

  ;; (defcondition <stream-error> <error>) but abstract
  (defclass <stream-error> (<error>)
    ()
    abstractp: t)

  (defcondition <format-error> <stream-error>)

  (define (format-error msg val)
          (error msg <format-error> value: val))

  (define (format stream string . args)
          (if (not (or (stream? stream)
                       (eq? stream t)
                       (eq? stream ())))
              (format-error "not a valid stream for format" stream)
            (let ((result (format-loop string args
                                       0 (string-length string) ())))
              (cond ((eq? stream t)
                     (%display result))
                    ((eq? stream ())
                     result)
                    (t
                      (%display result stream))))))

  (define (format-loop string args n len result)
          (if (< n len)
              (let ((char (string-ref string n)))
                (if (eqv? char escape-char)
                    (format-escape string args (+ n 1) len result)
                  (format-loop string args (+ n 1) len
                               (cons char result))))
            (list->string (reverse-list result))))

  (define (format-escape string args n len result)
          (if (< n len)
              (let* ((char (string-ref string n))
                     (op (table-ref escape-table char)))
                (if (null? op)
                    (format-error "unknown escape in format"
                                  (list->string (list #\~ char)))
                  (op char string args (+ n 1) len result)))
            (format-error "misplaced escape at end of format string" string)))

  (deflocal escape-table (make-table eqv?))
  (deflocal escape-arg-table (make-table eqv?))

  (define (check-args args string)
          (if (atom? args)
              (format-error "not enough args for format" string)
            (car args)))

  ; a prin arg
  (define (escape-a char string args n len result)
          (check-args args string)
          (format-loop string (cdr args) n len
                       (append (prin-to-string (car args)) result)))

  ((setter table-ref) escape-table #\a escape-a)

  ; b binary integer
  (define (binary-int char string args n len result)
          (check-args args string)
          (format-loop string (cdr args) n len
                       (append (radix (car args) 2) result)))

  ((setter table-ref) escape-table #\b binary-int)

  ; c character
  (define (escape-c char string args n len result)
          (let ((ch (check-args args string)))
            (if (not (char? ch))
                (format-error "not a character for ~c in format" ch)
              (escape-s char string args n len result))))

  ((setter table-ref) escape-table #\c escape-c)

  ; d decimal integer
  (define (decimal-int char string args n len result)
          (check-args args string)
          (format-loop string (cdr args) n len
                       (append (radix (car args) 10) result)))

  ((setter table-ref) escape-table #\d decimal-int)

  ; e fixed floating point
  ; f exponential floating point
  ; g general floating point
  (define (escape-float char string args n len result)
          (escape-arg-float char "" string args n len result))

  ((setter table-ref) escape-table #\e escape-float)
  ((setter table-ref) escape-table #\f escape-float)
  ((setter table-ref) escape-table #\g escape-float)

  (define (escape-arg-float char arg string args n len result)
          (check-args args string)
          (format-loop string (cdr args) n len
                       (append (reverse-list
                                 (string->list (xsprintf arg char (car args))))
                               result)))

  ((setter table-ref) escape-arg-table #\e escape-arg-float)
  ((setter table-ref) escape-arg-table #\f escape-arg-float)
  ((setter table-ref) escape-arg-table #\g escape-arg-float)

  ; o octal
  (define (octal-int char string args n len result)
          (check-args args string)
          (format-loop string (cdr args) n len
                       (append (radix (car args) 8) result)))

  ((setter table-ref) escape-table #\o octal-int)

  ; r radix
  (define (escape-r char string args n len result)
          (format-error "missing count for radix in format" string))

  ((setter table-ref) escape-table #\r escape-r)
  ((setter table-ref) escape-table #\R escape-r)

  (define (escape-int-r char arg string args n len result)
          (let ((count (string->number arg)))
            (if (not (integer? count))
                (format-error "bad base for ~r in format" arg)
              (begin
                (check-args args string)
                (format-loop string (cdr args) n len
                             (append (radix (car args) count)
                                     result))))))

  ((setter table-ref) escape-arg-table #\r escape-int-r)

  ; s write arg
  (define (escape-s char string args n len result)
          (check-args args string)
          (format-loop string (cdr args) n len
                       (append (write-to-string (car args)) result)))

  ((setter table-ref) escape-table #\s escape-s)

  ; t tab
  (define (escape-tab char string args n len result)
          (escape-int-tab char "1" string args n len result))

  ((setter table-ref) escape-table #\t escape-tab)

  (define (escape-int-tab char arg string args n len result)
          (let ((count (string->number arg)))
            (if (not (integer? count))
                (format-error "bad count for ~t in format" arg)
              (format-loop string args n len
                           (n-tabs count result)))))

  (define (n-tabs count result)
          (if (> count 0)
              (n-tabs (- count 1) (cons #\tab result))
            result))

  ((setter table-ref) escape-arg-table #\t escape-int-tab)

  ; x hexadecimal integer
  (define (hex-int char string args n len result)
          (check-args args string)
          (format-loop string (cdr args) n len
                       (append (radix (car args) 16) result)))

  ((setter table-ref) escape-table #\x hex-int)

  ; % newline
  (define (escape-% char string args n len result)
          (format-loop string args n len
                       (cons #\newline result)))

  ((setter table-ref) escape-table #\% escape-%)

  ; & conditional newline
  ((setter table-ref) escape-table #\& escape-%)

  ; | newpage
  (define (escape-pipe char string args n len result)
          (format-loop string args n len
                       (cons #\formfeed result)))

  ((setter table-ref) escape-table #\| escape-pipe)

  ; escape
  (define (escape-escape char string args n len result)
          (format-loop string args n len
                       (cons escape-char result)))

  ((setter table-ref) escape-table escape-char escape-escape)

  (define (escape-arg char string args n len result)
          (let* ((end (get-arg-end string (- n 1) len))
                 (arg (substring string (- n 1) end))
                 (char (string-ref string end))
                 (op (table-ref escape-arg-table char)))
            (if (null? op)
                (format-error "unknown escape in format" char)
              (op char arg string args (+ end 1) len result))))

  (define (check-string-ref string n len)
          (if (= n len)
              (format-error "missing op in format" string)
            (string-ref string n)))

  (define (get-arg-end string n len)
          (letrec ((loop1
                     (lambda (ind)
                       (let ((ch (check-string-ref string ind len)))
                         (cond ((eqv? ch #\.)
                                (loop2 (+ ind 1)))
                               ((memv ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                                (loop1 (+ ind 1)))
                               (t ind)))))
                   (loop2
                     (lambda (ind)
                       (let ((ch (check-string-ref string ind len)))
                         (if (memv ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                             (loop2 (+ ind 1))
                           ind)))))
                  (loop1 n)))

  (define (char->int char)
          (cdr (assv char
                     '((#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4) (#\5 . 5)
                       (#\6 . 6) (#\7 . 7) (#\8 . 8) (#\9 . 9) (#\0 . 0)))))

  ((setter table-ref) escape-table #\0 escape-arg)
  ((setter table-ref) escape-table #\1 escape-arg)
  ((setter table-ref) escape-table #\2 escape-arg)
  ((setter table-ref) escape-table #\3 escape-arg)
  ((setter table-ref) escape-table #\4 escape-arg)
  ((setter table-ref) escape-table #\5 escape-arg)
  ((setter table-ref) escape-table #\6 escape-arg)
  ((setter table-ref) escape-table #\7 escape-arg)
  ((setter table-ref) escape-table #\8 escape-arg)
  ((setter table-ref) escape-table #\9 escape-arg)
  ((setter table-ref) escape-table #\. escape-arg)

  (define (radix n base)
          (cond ((or (not (integer? base)) (< base 1) (> base 36))
                 (format-error "bad base in format" base))
                ((integer? n)
                 (cond ((= n 0) (list #\0))
                       ((< n 0) (append (convert-radix (- n) base) (list #\-)))
                       (t (convert-radix n base))))
                (format-error "expecting an integer in format" n)))

  (define (convert-radix n base)
          (if (= n 0)
              ()
            (cons (int->char (remainder n base))
                  (convert-radix (quotient n base) base))))

  (define (int->char n)
          (string-ref "0123456789abcdefghijlkmnopqrstuvwxyz" n))

  (define (prin-to-string obj)
          (output-to-string display obj))

  (define (write-to-string obj)
          (output-to-string write obj))

  ; cheating
  (define (output-to-string fn obj)
          (let ((stream (tmpfile)))
            (fn obj stream)
            (set-file-position! stream 0 0)     ; seek-set
            (let ((s (read-string stream '())))
              (close-stream stream)
              s)))

  (define (read-string stream sofar)
          (let ((ch (read-char stream)))
            (if (eof-object? ch)
                sofar
              (read-string stream (cons ch sofar)))))

  )
