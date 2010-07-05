;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: boot module
;;;-----------------------------------------------------------------------------
(defmodule boot1
  (syntax (_boot0)
   export (car cdr cons list
           string? character? symbol? cons? int? int? list? atom? null?
           simple-function? simple-generic-function? int-zero? object?
           + - * / % mod < = inc dec
           eq eql equal
           format write-object sprin prin sprint print
           make-symbol make-keyword
           make-vector make-vector1 vector-size vector-ref
           member1-string string-ref string-size substring tailstring
           character-as-int int-as-character
           else
           stdout stderr setter
           *argc* *argv* getenv system exit time-start time-stop
           *absent*))

;;;-----------------------------------------------------------------------------
;;; Setter
;;;-----------------------------------------------------------------------------
  (defun setter (fun)
    ((opencoded-lambda (f) (setter)) fun))

  (defun set-setter (fun1 fun2)
    ((opencoded-lambda (f1 f2) (set-setter)) fun1 fun2))

  (set-setter setter set-setter)

;;;-----------------------------------------------------------------------------
;;; Equality
;;;-----------------------------------------------------------------------------
  (defun eq (x y) ((opencoded-lambda (u v) (eq)) x y))
  (declare-inline eq)

  (defun eql (x y) ((opencoded-lambda (u v) (eql)) x y))
  (declare-inline eql)

  (defun equal (x y)
    (labels
     ((loop (l1 l2)
            (if (and (cons? l1) (cons? l2))
                (and (equal (car l1) (car l2))
                     (loop (cdr l1) (cdr l2)))
              (if l1
                  (and l2 (eql l1 l2))
                (null? l2)))))
      (loop x y)))

;;;-----------------------------------------------------------------------------
;;; Class membership
;;;-----------------------------------------------------------------------------
  (defun null? (x) ((opencoded-lambda (u) (null)) x))
  (declare-inline null?)

  (defun character? (x) ((opencoded-lambda (u) (characterp)) x))
  (declare-inline character?)

  (defun string? (x) ((opencoded-lambda (u) (stringp)) x))
  (declare-inline string?)

  (defun symbol? (x) ((opencoded-lambda (u) (symbolp)) x))
  (declare-inline symbol?)

  (defun cons? (x) ((opencoded-lambda (u) (consp)) x))
  (declare-inline cons?)

  (defun int? (x) ((opencoded-lambda (u) (fpip)) x))
  (declare-inline int?)

  (defun simple-function? (x) ((opencoded-lambda (u) (lambdap)) x))
  (declare-inline simple-function?)

  (defun simple-generic-function? (x) ((opencoded-lambda (u) (gfp)) x))
  (declare-inline simple-generic-function?)

  (defun list? (x) ((opencoded-lambda (u) (listp)) x))
  (declare-inline list?)

  (defun atom? (x) (null? (cons? x)))
  (declare-inline atom?)

;;;-----------------------------------------------------------------------------
;;; Arithmetic
;;;-----------------------------------------------------------------------------
  (defun + (x y) ((opencoded-lambda (x y) (fpi-sum)) x y))
  (declare-inline +)

  (defun - (x y) ((opencoded-lambda (x y) (fpi-difference)) x y))
  (declare-inline -)

  (defun * (x y) ((opencoded-lambda (x y) (fpi-product)) x y))
  (declare-inline *)

  (defun / (x y) ((opencoded-lambda (x y) (fpi-quotient)) x y))
  (declare-inline /)

  (defun % (x y) ((opencoded-lambda (x y) (fpi-remainder)) x y))
  (declare-inline %)

  (defun mod (x y) ((opencoded-lambda (x y) (fpi-remainder)) x y))
  (declare-inline mod)

  (defun < (x y) ((opencoded-lambda (x y) (fpi-lt)) x y))
  (declare-inline <)

  (defun = (x y) ((opencoded-lambda (x y) (fpi-equal)) x y))
  (declare-inline =)

  (defun inc (x) ((opencoded-lambda (x) (fpi-inc)) x))
  (declare-inline inc)

  (defun dec (x) ((opencoded-lambda (x) (fpi-dec)) x))
  (declare-inline dec)

  (defun int-zero? (x) ((opencoded-lambda (x) (fpi-zerop)) x))
  (declare-inline int-zero?)

;;;-----------------------------------------------------------------------------
;;; Lists
;;;-----------------------------------------------------------------------------
  (defun car (l) ((opencoded-lambda (ll) (car)) l))
  (declare-inline car)

  (defun cdr (l) ((opencoded-lambda (ll) (cdr)) l))
  (declare-inline cdr)

  (defun (setter car) (obj x)  ; inlined when car inlined
    ((opencoded-lambda (u v) (set-car)) obj x))

  (defun (setter cdr) (obj x)  ; inlined when cdr inlined
    ((opencoded-lambda (u v) (set-cdr)) obj x))

  (defun cons (x y) ((opencoded-lambda (u v) (cons)) x y))
  (declare-inline cons)

  (defun list l l)

;;;-----------------------------------------------------------------------------
;;; Print/format
;;;-----------------------------------------------------------------------------
  (defconstant stdout 1)
  (defconstant stderr 2)
  (defopencoded write-object (x fd) (write-object))

  (defun sprin (s x)
    (write-object x s))

  (defun prin (x)
    (sprin stdout x))

  (defun sprint (s x)
    (write-object x s)
    (write-object "\n" s))

  (defun print (x)
    (sprint stdout x))

  (defun format (fd str . l)
    (let ((n (string-size str))
          (i 0))
      (labels
       ((loop (ll)
              (if (< i n)
                  (let ((c (string-ref str i)))
                    (cond
                     ((eql c #\~)
                      (setq c (string-ref str (+ i 1)))
                      (setq i (+ i 2))
                      (cond
                       ((eq c #\a) (write-object (car ll) fd) (loop (cdr ll)))
                       ((eq c #\~) (write-object #\~ fd) (loop ll))
                       (t (write-object c fd) (loop ll))))
                     (t (write-object c fd) (setq i (+ i 1)) (loop ll))))
                ll)))
       (loop l))))

;;;-----------------------------------------------------------------------------
;;; Strings and symbols
;;;-----------------------------------------------------------------------------
  (defun string-size (obj) ((opencoded-lambda (o) (primitive-size)) obj))
  (declare-inline string-size)

  (defun string-ref (str i) ((opencoded-lambda (s i) (string-ref)) str i))
  (declare-inline string-ref)

  (defun (setter string-ref) (str i c)
    ((opencoded-lambda (s i c) (set-string-ref)) str i c))

  (defopencoded character-as-int (x) (character-as-fpi))
  (defopencoded int-as-character (i) (fpi-as-character))

  (defextern substring (<string> <int> <int>) <string> "eul_substr")
  (defextern tailstring (<string> <int>) <string> "eul_tailstr")
  (defextern member1-string (<character> <string>) ptr "eul_str_member1")
  (defextern make-symbol (<string>) ptr "eul_make_symbol")
  (defextern make-keyword (<string>) ptr "eul_make_keyword")

;;;-----------------------------------------------------------------------------
;;; Vectors
;;;-----------------------------------------------------------------------------
  (defextern make-vector1 (<int> ptr) ptr "eul_make_vector")

  (defun make-vector (n . init) (make-vector1 n init))

  (defun vector-size (vec) ((opencoded-lambda (x) (primitive-size)) vec))
  (declare-inline vector-size)

  (defun vector-ref (vec index)
    ((opencoded-lambda (x i) (primitive-ref)) vec index))
  (declare-inline vector-ref)

  (defun (setter vector-ref) (vec index value)
    ((opencoded-lambda (x i v) (set-primitive-ref)) vec index value))

;;;-----------------------------------------------------------------------------
;;; Object?
;;;-----------------------------------------------------------------------------
  (defun object? (x) (eul_is_object x))

;;;-----------------------------------------------------------------------------
;;; An alias for `t' for the default `cond' clause
;;;-----------------------------------------------------------------------------
  (defconstant else t)

;;;-----------------------------------------------------------------------------
;;; The absent value
;;;-----------------------------------------------------------------------------
  (defconstant *absent* '(*absent*))

;;;-----------------------------------------------------------------------------
;;; Main's argc, argv
;;;-----------------------------------------------------------------------------
  (defopencoded getargc () (register-ref argc))
  (defopencoded getargv() (register-ref argv))
  (defconstant *argc* (getargc))
  (defconstant *argv* (getargv))

;;;-----------------------------------------------------------------------------
;;; Getenv, system and time
;;;-----------------------------------------------------------------------------
  (defextern eul_getenv (<string>) <string> "getenv")

  (defun getenv (str) (eul_getenv str))

  (defextern eul_system (<string>) <int> "system")
  (defun system (str) (eul_system str))

  (defextern eul_time_start () ptr)
  (defun time-start () (eul_time_start))

  (defextern eul_time_stop (ptr) ptr)
  (defun time-stop (x) (eul_time_stop x))

  (defextern eul_is_object (ptr) ptr)

;;;-----------------------------------------------------------------------------
;;; Exit
;;;-----------------------------------------------------------------------------
  (defun exit x ((opencoded-lambda (x) (exit)) x))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
