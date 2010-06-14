;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: scheme
;;;  Authors: Andreas Kind, Luc Moreau
;;; Description: Scheme (IEEE Std 1178-1990) in EuLisp
;;;-----------------------------------------------------------------------------
(defmodule scheme
  (syntax ((rename ((not Not)) macros))
   import ((rename ((/ quotient)(member Member)(read Read)
                    (error Error) (random Random)) level1)
           math bignum))

;;;-----------------------------------------------------------------------------
;;; To be removed
;;;-----------------------------------------------------------------------------
  (defmethod (converter <double>) ((x <double>)) x)

;;;-----------------------------------------------------------------------------
;;; Not yet implemented
;;;-----------------------------------------------------------------------------
  (defun nyi (name)
    (Error "Scheme function ~a not yet implemented" name))

;;;-----------------------------------------------------------------------------
;;; Constants
;;;-----------------------------------------------------------------------------
  (defconstant #t t)
  (defconstant else t)
  (defconstant #f ())
  (defconstant false ())
  (defconstant true t)
  ;(defconstant |#\newline| #\\n)
  ;(defconstant |#\space| #\\s)

;;;-----------------------------------------------------------------------------
;;; Functions are not renamed to get correct print names
;;;-----------------------------------------------------------------------------
  (export * + -)

  (defun / (x y)
    (let ((q (binary/ x y))
          (r (binary% x y)))
      (if (= r 0)
          q
        (binary/ (convert x <double-float>) y))))

  (export < <= = > >= abs acos)

  (defun angle args (nyi 'angle))

  (export append apply)

  (defun approximate args (nyi 'approximate))

  (export asin)

  (defun assoc (x y)
    (assoc-list-ref y x 'equal))

  (defun assq (key a-list)
    (assoc-list-ref a-list key))
  (declare-inline assq)

  (defun assv (x y)
    (assoc-list-ref y x 'eql))

  (export atan)

  (defun boolean? (x)
    (if x (eq x '#t) t))
  (declare-inline boolean?)

  (export caar caddr cadr)

  (defun call-with-current-continuation (x)
    (call1/cc x))
  (declare-inline call-with-current-continuation)

  (defun call/cc (x)
    (call1/cc x))
  (declare-inline call/cc)

  (defun call-with-input-file args
    (nyi 'call-with-input-file))

  (defun call-with-output-file args
    (nyi 'call-with-input-file))

  (export car cddr cdddr)

  (defun cdddar (x)
    (cdr (cddar x)))
  (declare-inline cdddar)

  (defun cddddr (x)
    (cdr (cdddr x)))
  (declare-inline cddddr)

  (export cdr ceiling)

  (defun char->integer (x)
    (character-as-int x))
  (declare-inline char->integer)

  (defun char-alphabetic? (x)
    (alphap x))
  (declare-inline char-alphabetic?)

  (defun char-ci<=? args
    (nyi 'char-ci<=?))

  (defun char-ci<? args
    (nyi 'char-ci<?))

  (defun char-ci=? args
    (nyi 'char-ci=?))

  (defun char-ci>=? args
    (nyi 'char-ci>=?))

  (defun char-ci>? args
    (nyi 'char-ci>?))

  (defun char-downcase (x)
    (as-lowercase x))
  (declare-inline char-downcase)

  (defun char-lower-case? (x)
    (lowercase? x))
  (declare-inline char-lower-case?)

  (defun char-numeric? (x)
    (digit? x))
  (declare-inline char-numeric?)

  (defun char-ready args
    (nyi 'char-ready))

  (defun char-ready? args
    (nyi 'char-ready?))

  (defun char-upcase (x)
    (as-uppercase x))
  (declare-inline char-upcase)

  (defun char-upper-case? (x)
    (uppercase? x))
  (declare-inline char-upper-case?)

  (defun char-whitespace? args
    (nyi 'char-whitespace?))

  (defun char<=? (x y)
    (<= x y))
  (declare-inline char<=?)

  (defun char<? (x y)
    (< x y))
  (declare-inline char<?)

  (defun char=? (x y)
    (eql x y))
  (declare-inline char=?)

  (defun char>=? (x y)
    (>= x y))
  (declare-inline char>=?)

  (defun char>? (x y)
    (< y x))
  (declare-inline char>?)

  (defun char? (x)
    (character? x))
  (declare-inline char?)

  (defun close-input-port (x)
    (disconnect x))
  (declare-inline close-input-port)

  (defun close-output-port (x)
    (disconnect x))
  (declare-inline close-output-port)

  (defun complex? args
    (nyi 'complex?))

  (export cons)

  ;  (defun construct-identifier args
  ;    (nyi 'construct-identifier))

  (export cos)

  (defun current-input-port ()
    stdin)
  (declare-inline current-input-port)

  (defun current-output-port ()
    stdout)
  (declare-inline current-output-port)

  (export denumerator)

  (defun display (x . y)
    (apply prin x y)
    (apply flush y))

  (defun eof-object? (x)
    (eq (eos-default-value) x))

  (defun eq? (x y)
    (eq x y))
  (declare-inline eq?)

  (defun equal? (x y)
    (binary= x y))
  (declare-inline equal?)

  (defun eqv? (x y)
    (eql x y))
  (declare-inline eqv?)

  (defun even? (x)
    (evenp x))
  (declare-inline even?)

  (defun error (str x)
    (Error "~a: ~a" str x))

  (defun exact->inexact (x)
    (convert x <double-float>))

  (defun exact? (x)
    (intp x))
  (declare-inline exact?)

  (export exp)

  (defun expt args
    (nyi 'expt))

  (export floor)

  (defun for-each (x y)
    (do1-list x y))
  (declare-inline for-each)

  ;  (defun free-identifier=? args

  ;    (nyi 'free-dentifier=?))

  (export gcd)

  ;  (defun gen-counter args
  ;    (nyi 'gen-counter))

  ;  (defun gen-looser args
  ;    (nyi 'gen-looser))

  ;  (defun generate-identifier x
  ;    (apply gensym x))

  ;  (defun identifier->symbol (x)
  ;    x)

  ;  (defun identifier? (x)
  ;    (symbolp x))
  ;  (declare-inline identifier?)

  (defun imag-part args
    (nyi 'imag-part))

  (defun inexact->exact (x)
    (convert x <int>))

  (defun inexact? (x)
    (doublep x))
  (declare-inline inexact?)

  (defun input-port? (x)
    (and (streamp x)
         (Member (stream-mode x) '(r rw))))

  (defun integer->char (x)
    (int-as-character x))
  (declare-inline integer->char)

  (defun last-pair (x)
    (last x))
  (declare-inline last-pair)

  (export lcm)

  (defun length (x)
    (size x))
  (declare-inline length)

  (export list)

  (defun list->string (x)
    (convert x <string>))

  (defun list->vector (l)
    (apply make-vector (list-size l) l))

  (export list-ref)

  ;  (defun list-tail args
  ;    (nyi 'list-tail))

  (defun list? (x)
    (listp x))
  (declare-inline list?)
  (export log)

  (defun magnitude args
    (nyi 'magnitude))

  (defun make-polar args
    (nyi 'make-polar))

  (defun make-string (n . c)
    (if c
        (make <string> size: n fill-value: (car c))
      (make <string> size: n)))

  (export make-vector map max)

  (defun member (x y)
    (Member x y binary=))

  (defun memq (x y)
    (Member x y eq))

  (defun memv (x y)
    (Member x y eql))

  (export min)

  (defun modulo (x y)
    (binary% x y))
  (declare-inline modulo)

  (defun negative? (x)
    (negative? x))
  (declare-inline negative?)
  (export newline)

  (defun not (x)
    (null? x))
  (declare-inline not)

  (defun number? (x)
    (number? x))
  (declare-inline number?)

  (defun number->string (x . r)
    (let ((radix (if r (car r) 10)))
      (cond ((= radix 10)
             (sprintf "%i" x))
            ((= radix 16)
             (sprintf "%x" x))
            ((= radix 8)
             (sprintf "%o" x))
            ;           ((= radix 2)
            ;            )
            (t
             (Error "bad radix ~a in number->string" radix)))))

  (export numerator)

  (defun odd? (x)
    (oddp x))
  (declare-inline odd?)

  (defun open-input-file (x)
    (make <file-stream> mode: 'r file-name: x))

  (defun open-output-file (x)
    (make <file-stream> mode: 'w file-name: x))

  (defun output-port? (x)
    (and (streamp x)
         (Member (stream-mode x) '(w rw a))))

  (defun pair? (x)
    (consp x))
  (declare-inline pair?)

  (defun peek-char args
    (nyi 'peek-char))

  (defun positive? (x)
    (positive? x))
  (declare-inline positive?)

  (defun procedure? (x)
    (function? x))
  (declare-inline procedure?)

  ;; quasiquote, quote

  (defun random (x)
    (Random x))

  (defun rational? (x)
    (bigratp x))
  (declare-inline rational?)

  (defun rationalize (x)
    (make <bigrat> value: x))

  (defun read x
    (apply read-s-expression x))
  (export read-char)

  (defun real-part args
    (nyi 'real-part))

  (defun real? (x)
    (float? x))
  (declare-inline real?)

  (defun remainder (x y)
    (binary% x y))
  (declare-inline remainder)

  (export reverse round)

  ;  (defun remq (x y)
  ;    (list-remove x y))
  ;  (declare-inline remq)

  ;  (defun remq! (x y)
  ;    (cond
  ;     ((null? y) y)
  ;     ((eq x (car y)) (remq! x (cdr y)))
  ;     (t (let loop ((prev y))
  ;            (cond ((null? (cdr prev))
  ;                   y)
  ;                  ((eq (cadr prev) x)
  ;                   ((setter cdr) prev (cddr prev))
  ;                   (loop prev))
  ;                  (t (loop (cdr prev))))))))

  (defun runtime ()
    (vector-ref (time-start) 0))

  (defun set-car! (x y)
    ((setter car) x y))
  (declare-inline set-car!)

  (defun set-cdr! (x y)
    ((setter cdr) x y))
  (declare-inline set-cdr!)

  (export sin sqrt)

  (defun string x
    (convert x <string>))

  (defun string? (x)
    (stringp x))

  (declare-inline string?)

  (defun string->number (x)
    ;; floats not addressed!
    (convert x <int>))
  (declare-inline string->number)

  (defun string->symbol (str)
    (make <symbol> name: str))
  (declare-inline string->symbol)

  (export string-append)

  ;; string-ci*?

  (defun string-length (x)
    (string-size x))
  (declare-inline string-length)

  (export string-ref)

  (defun string-set! (x y z)
    ((setter string-ref) x y z))
  (declare-inline string-set!)

  (defun string<= (x y)
    (<= x y))
  (declare-inline string<=)

  (defun string< (x y)
    (< x y))
  (declare-inline string<)

  (defun string= (x y)
    (string-equal x y))
  (declare-inline string=)

  (defun string>= (x y)
    (>= x y))
  (declare-inline string>=)

  (defun string> (x y)
    (< y x))
  (declare-inline string>)

  (defun string? (x)
    (stringp x))
  (declare-inline string?)

  (export substring) ;; indexing may be different!

  (defun symbol->string (x)
    (symbol-name x))
  (declare-inline symbol->string)

  (defun symbol? (x)
    (symbolp x))
  (declare-inline symbol?)

  (export tan truncate)

  ;; unquote, unquote-splicing

  (defun vector x
    (list->vector x))

  (defun vector? (x)
     (vector? x))
  (declare-inline vector?)

  (defun vector-length (x)
    (vector-size x))
  (declare-inline vector-length)

  (export vector-ref)

  (defun vector-set! (x y z)
    ((setter vector-ref) x y z))
  (declare-inline vector-set!)

  (defun vector? (x)
    (vector? x))
  (declare-inline vector?)

  (export write)

  (defun write-char (x . y)
    ;; not optimal!
    (apply write x y))
  (declare-inline write-char)

  (defun zero? (x)
    (zero? x))
  (declare-inline zero?)

;;;-----------------------------------------------------------------------------
;;; Some youtoo bindings the compiler expects
;;;-----------------------------------------------------------------------------
  (export int-binary+)
  (export int-binary-)
  (export int-binary*)
  (export int-binary/)
  (export int-binary%)
  (export int-binary<)
  (export int-binary=)
  (export inc)
  (export dec)
  (export int-zerop)

;;;-----------------------------------------------------------------------------
;;; Export everything
;;;-----------------------------------------------------------------------------
  (export above:)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
