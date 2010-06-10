;;; copy.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule copy

    (import (root))

  (define (identity x) x)

  (define-generic (deep-copy obj))

  (define-method (deep-copy (obj <object>))
                 obj)

  (define-generic (shallow-copy obj))

  (define-method (shallow-copy (obj <object>))
                 obj)

  (export deep-copy shallow-copy)

  ;; structure classes
  (define-method (deep-copy (s <structure>))
                 (let ((cl (class-of s)))
                   (structure-copy-loop
                     (allocate cl ())
                     s
                     1
                     (class-instance-size cl)
                     deep-copy)))

  (define-method (shallow-copy (s <structure>))
                 (let ((cl (class-of s)))
                   (structure-copy-loop
                     (allocate cl ())
                     s
                     1
                     (class-instance-size cl)
                     identity)))

  (define (structure-copy-loop new old index len copy)
          (if (<= index len)
              (begin
                (setivar new index (copy (getivar old index)))
                (structure-copy-loop new old (+ index 1) len copy))
            new))

  ;; conses
  (define-method (deep-copy (c <cons>))
                 (cons-copy-loop c deep-copy))

  (define-method (shallow-copy (c <cons>))
                 (cons-copy-loop c identity))

  (define (cons-copy-loop c copy)
          (if (atom? c)
              (copy c)
            (cons (copy (car c))
                  (cons-copy-loop (cdr c) copy))))

  ;; strings
  (define-method (deep-copy (s <simple-string>))
                 (shallow-copy s))

  (define-method (shallow-copy (s <simple-string>))
                 (list->string (string->list s)))

  ;; characters
  (define-method (deep-copy (c <simple-char>))
                 (shallow-copy c))

  (define-method (shallow-copy (c <simple-char>))
                 (integer->char (char->integer c)))

  ;; vectors
  (define-method (deep-copy (v <simple-vector>))
                 (let ((len (vector-length v) ))
                   (vector-copy-loop
                     (make-vector len ())
                     v
                     0
                     len
                     deep-copy)))

  (define-method (shallow-copy (v <simple-vector>))
                 (let ((len (vector-length v) ))
                   (vector-copy-loop
                     (make-vector len ())
                     v
                     0
                     len
                     identity)))

  (define (vector-copy-loop new old index len copy)
          (if (< index len)
              (begin
                (vector-set! new index (copy (vector-ref old index)))
                (vector-copy-loop new old (+ index 1) len copy))
            new))

  ;; tables
  (define-method (deep-copy (t <hash-table>))
                 (let ((new (make-table (table-comparator t) (deep-copy (table-fill t)))))
                   (map-list
                     (lambda (k v)
                       ((setter table-ref) new k (deep-copy v)))
                     (table-keys t)
                     (table-values t))
                   new))

  (define-method (shallow-copy (t <hash-table>))
                 (let ((new (make-table (table-comparator t) (table-fill t))))
                   (map-list
                     (lambda (k v)
                       ((setter table-ref) new k v))
                     (table-keys t)
                     (table-values t))
                   new))

  ;; floats
  (define-method (deep-copy (f <double-float>))
                 (shallow-copy f))

  (define-method (shallow-copy (f <double-float>))
                 (+ f 0.0))

  )
