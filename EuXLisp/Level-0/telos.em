;;; Copyright 1994 Russell Bradford
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                           EuLisp System 'EuXLisp'
;;;-----------------------------------------------------------------------------
;;
;;  EuXLisp is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: EuLisp Level-0 telos module
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule telos
  (syntax (syntax)
   import (root
           telosint)
   export (;; classes
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
           <stream>
           <input-stream>
           <output-stream>
           <i/o-stream>
           <vector>
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

           generic-print
           sprint
           print
           nl

           generic-write
           swrite
           write

           wait

           ; specials
           defclass
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
   export (;; from telosint, while developing
           class-of
           class-name
           class-superclasses
           class-precedence-list
           class-slots
           class-keywords
           ;   set-class-keywords
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
           ;   set-slot-default
           slot-required?
           ;   set-slot-required?
           ;   find-slot-index
           ;   initialize-object
           ;   initialize-class
           ))

;;;-----------------------------------------------------------------------------
;;; Class make and initialisation functions
;;;-----------------------------------------------------------------------------
(defun make (cl . inits)
  (let ((maker (table-ref builtin-make-table cl)))
    (if (null? maker)
        (initialize (allocate cl inits) inits)
      (maker inits))))

(defgeneric initialize (cl inits))

(defmethod initialize ((obj <object>) inits)
  (initialize-object obj inits))

(defmethod initialize ((cl <class>) inits)
  (call-next-method)
  (initialize-class cl inits))

;;;-----------------------------------------------------------------------------
;;; Class introspection
;;;-----------------------------------------------------------------------------
(defun class-hierarchy top
  (hierarchy (if (null? top) <object> (car top)) 0)
  t)

(defun hierarchy (cl depth)
  (if (class-abstract? cl)
      (%print "A  ")
    (%print "   "))
  (indent depth)
  (%print cl)
  (print nl)
  (for-each
   (lambda (c)
     (hierarchy c (%+ depth 2)))
   (reverse-list (class-subclasses cl))))

(defun indent (n)
  (while (%> n 0)
    (%print " ")
    (setq n (%- n 1))))


;;;-----------------------------------------------------------------------------
;;; Printing
;;;-----------------------------------------------------------------------------

; Convenient character constant for printing a newline
(defconstant nl #\\n)

;;;-----------------------------------------------------------------------------
;;;  Generic print functions
;;;-----------------------------------------------------------------------------
(defgeneric generic-print (obj s))

(defmethod generic-print ((obj <object>) s)
  (%print obj s)
  obj)

(defmethod generic-print ((obj <null>) s)
  (%print "()" s)
  obj)

(defmethod generic-print ((obj <list>) s)
  (write-list obj s generic-print))

(defmethod generic-print ((obj <vector>) s)
  (write-vector obj s generic-print))

;;;-----------------------------------------------------------------------------
;;;  N-ary print functions
;;;-----------------------------------------------------------------------------
(defun sprint-1 (s objs)
  (for-each (lambda (x) (generic-print x s)) objs)
  s)

(defun sprint (s . objs)
  (sprint-1 s objs))

(defun print objs
  (sprint-1 stdout objs))

;;;-----------------------------------------------------------------------------
;;;  Generic write functions
;;;-----------------------------------------------------------------------------
(defgeneric generic-write (obj s))

(defmethod generic-write ((obj <object>) s)
  (%write obj s)
  obj)

(defmethod generic-write ((obj <null>) s)
  (%print "()" s)
  obj)

(defmethod generic-write ((obj <list>) s)
  (write-list obj s generic-write))

(defmethod generic-write ((obj <vector>) s)
  (write-vector obj s generic-write))

;;;-----------------------------------------------------------------------------
;;;  N-ary write functions
;;;-----------------------------------------------------------------------------
(defun swrite-1 (s objs)
  (for-each (lambda (x) (generic-write x s)) objs)
  s)

(defun swrite (s . objs)
  (swrite-1 s objs))

(defun write objs
  (swrite-1 stdout objs))

;;;-----------------------------------------------------------------------------
;;;  Ancillary write functions
;;;-----------------------------------------------------------------------------

;; A feeble attempt at stopping infinite loops
(deflocal current-print-depth 0)
(defun inc-pr-depth (n)
  (setq current-print-depth (%+ current-print-depth n)))

;; Maintain tail recursion in write-list1
(defun write-list (obj s gfun)
  (cond ((and (print-depth)
              (%>= current-print-depth (print-depth)))
         (%print "(...)" s))
        ((list? obj)
         (%print "(" s)
         (inc-pr-depth 1)
         (gfun (car obj) s)
         (write-list1 (cdr obj) s gfun 1)
         (inc-pr-depth -1))
        (t (%write obj s)))        ; new subclass of <list>
  obj)


(defun write-list1 (obj s gfun current-print-breadth)
  (cond ((null? obj)
         (%print ")" s))
        ((atom? obj)
         (%print " . " s)
         (gfun obj s)
         (%print ")" s))
        ((and (print-breadth)
              (%>= current-print-breadth (print-breadth)))
         (%print " ...)" s))
        (else
         (%print " " s)
         (gfun (car obj) s)
         (write-list1 (cdr obj) s gfun (%+ current-print-breadth 1)))))

(defun write-vector (obj s gfun)
  (cond ((and (print-depth)
              (%>= current-print-depth (print-depth)))
         (%print "#(...)" s))
        ((vector? obj)
         (let ((size (vector-size obj)))
           (if (%= size 0)
               (%print "#()" s)
             (progn
               (%print "#(" s)
               (inc-pr-depth 1)
               (gfun (vector-ref obj 0) s)
               (write-vector1 obj s 1 size gfun)
               (inc-pr-depth -1)))))
        (t (%write obj s)))        ; new subclass of <vector>
  obj)

(defun write-vector1 (obj s index size gfun)
  (if (%= index size)
      (%print ")" s)
    (progn
      (%print " " s)
      (gfun (vector-ref obj index) s)
      (write-vector1 obj s (%+ index 1) size gfun))))

;;;-----------------------------------------------------------------------------
;;; Wait functions
;;;-----------------------------------------------------------------------------
(defgeneric wait (thread timeout))

;;;-----------------------------------------------------------------------------
)  ;; End of module telos
;;;-----------------------------------------------------------------------------
