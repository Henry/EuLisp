;;; telos.em
;;; Euscheme code Copyright (c) 1994 Russell Bradford

(defmodule telos

    (import (root telosint))

  (define (make cl . inits)
          (let ((maker (table-ref builtin-make-table cl)))
            (if (null? maker)
                (initialize (allocate cl inits) inits)
              (maker inits))))

  (define-generic (initialize cl inits))

  (define-method (initialize (obj <object>) inits)
                 (initialize-object obj inits))

  (define-method (initialize (cl <class>) inits)
                 (call-next-method)
                 (initialize-class cl inits))

  ;; class-hierarchy
  (define (class-hierarchy . top)
          (hierarchy (if (null? top) <object> (car top)) 0)
          t)

  (define (hierarchy cl depth)
          (if (class-abstract? cl)
              (%display "A  ")
            (%display "   "))
          (indent depth)
          (%display cl)
          (newline)
          (for-each
            (lambda (c)
              (hierarchy c (+ depth 2)))
            (reverse-list (class-subclasses cl))))

  (define (indent n)
          (while (> n 0)
            (princ " ")
            (setq n (- n 1))))

  ; generic printing
  (define-generic (generic-prin obj s))

  (define-method (generic-prin (obj <object>) s)
                 (%display obj s)
                 obj)

  (define-method (generic-prin (obj <null>) s)
                 (%display "()" s)
                 obj)

  (define-method (generic-prin (obj <list>) s)
                 (write-list obj s generic-prin))

  (define-method (generic-prin (obj <vector>) s)
                 (write-vector obj s generic-prin))

  (define (sprin-all s objs)
          (for-each (lambda (x) (generic-prin x s)) objs)
          s)

  (define (sprin s . objs)
          (sprin-all s objs))

  (define (sprint s . objs)
          (sprin-all s objs)
          (newline s))

  (define (prin . objs)
          (sprin-all stdout objs))

  (deflocal %print print)

  (define (print . objs)
          (sprin-all stdout objs)
          (newline))

  (define-generic (generic-write obj s))

  (define-method (generic-write (obj <object>) s)
                 (%write obj s)
                 obj)

  (define-method (generic-write (obj <null>) s)
                 (%display "()" s)
                 obj)

  (define-method (generic-write (obj <list>) s)
                 (write-list obj s generic-write))

  (define-method (generic-write (obj <vector>) s)
                 (write-vector obj s generic-write))

  (define (swrite-all s objs)
          (for-each (lambda (x) (generic-write x s)) objs)
          s)

  (define (swrite s . objs)
          (swrite-all s objs))

  (deflocal %write write)

  (define (write . objs)
          (swrite-all stdout objs))

  ;; a feeble attempt at stopping infinite loops
  (deflocal current-print-depth 0)
  (define (inc-pr-depth n)
          (setq current-print-depth (+ current-print-depth n)))

  ;; maintain tail recursion in write-list1
  (define (write-list obj s gfun)
          (cond ((and (print-depth)
                      (>= current-print-depth (print-depth)))
                 (%display "(...)" s))
                ((list? obj)
                 (%display "(" s)
                 (inc-pr-depth 1)
                 (gfun (car obj) s)
                 (write-list1 (cdr obj) s gfun 1)
                 (inc-pr-depth -1))
                (t (%write obj s)))        ; new subclass of <list>
          obj)


  (define (write-list1 obj s gfun current-print-breadth)
          (cond ((null? obj)
                 (%display ")" s))
                ((atom? obj)
                 (%display " . " s)
                 (gfun obj s)
                 (%display ")" s))
                ((and (print-breadth)
                      (>= current-print-breadth (print-breadth)))
                 (%display " ...)" s))
                (else
                  (%display " " s)
                  (gfun (car obj) s)
                  (write-list1 (cdr obj) s gfun (+ current-print-breadth 1)))))

  (define (write-vector obj s gfun)
          (cond ((and (print-depth)
                      (>= current-print-depth (print-depth)))
                 (%display "#(...)" s))
                ((vector? obj)
                 (let ((length (vector-length obj)))
                   (if (= length 0)
                       (%display "#()" s)
                     (begin
                       (%display "#(" s)
                       (inc-pr-depth 1)
                       (gfun (vector-ref obj 0) s)
                       (write-vector1 obj s 1 length gfun)
                       (inc-pr-depth -1)))))
                (t (%write obj s)))        ; new subclass of <vector>
          obj)

  (define (write-vector1 obj s index length gfun)
          (if (= index length)
              (%display ")" s)
            (begin
              (%display " " s)
              (gfun (vector-ref obj index) s)
              (write-vector1 obj s (+ index 1) length gfun))))

  (define-generic (wait thread timeout))

  (export

   ; classes
   <object>
   <class>
   <simple-class>
   <list>
   <cons>
   <null>
   <number>
   <integer>
   <fpi>
   <float>
   <double-float>
   <symbol>
   <keyword>
   <string>
   <simple-string>
   <port>
   <input-port>
   <output-port>
   <i/o-port>
   <vector>
   <simple-vector>
   <char>
   <simple-char>
   <promise>
   <env>
   <code>
   <module>
   <table>
   <hash-table>
   <function>
   <simple-function>
   <closure>
   <subr>
   <continuation>
   <generic>
   <simple-generic>
   <xsubr>
   <csubr>
   <method>
   <simple-method>
   <slot>
   <local-slot>
   <structure>

   generic-prin
   sprin
   sprint
   prin
   print

   generic-write
   swrite
   write

   wait

   ; specials
   defclass
   generic-lambda
   call-next-method
   next-method?

   ; functions
   make
   allocate
   initialize

   ; debugging
   describe
   class-hierarchy

   current-print-depth
   )

  ;; from telosint, while developing
  (export

   class-of
   class-name
   class-superclasses
   class-precedence-list
   class-slots
   class-keywords
   ;   set-class-keywords!
   class-subclasses
   class-instance-size
   class-abstract?
   class?
   subclass?
   generic-name
   generic-args
   generic-methods
   generic-cache1
   generic-cache2
   make-generic
   make-method
   method-generic
   method-function
   method-domain
   add-method
   slot-name
   slot-keyword
   slot-default
   ;   set-slot-default!
   slot-required?
   ;   set-slot-required?!
   ;   find-slot-index
   ;   initialize-object
   ;   initialize-class

   )

  )
