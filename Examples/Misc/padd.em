;;; Copyright (c) 1996 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Authors: Andreas Kind, Liam Wickins
;;; Description: Addition of n numbers, in parallel
;;;  Compilation (default)
;;    youtoo padd -l level1
;;;  Compilation (with foreign thread library)
;;    youtoo padd -l level1 -l fthread
;;;-----------------------------------------------------------------------------
(defmodule padd
  (syntax (macros)
   import (level1)             ;; default
   ;; import (level1 fthread)     ;; with foreign thread library
   export (p-add))

  (defun p-add args
    ;; Expects at least two arguments
    (add-aux (spawn add-simple args)))

  (defun spawn (fun args)
    (if (null args)
        ()
      (let ((thrd (make <thread> function: fun)))
        (thread-start thrd (car args) (cadr args))
        (cons thrd (spawn fun (cddr args))))))

  (defun add-aux (thrds)
    (if (null (cdr thrds))
        (thread-value (car thrds))
      (add-aux (spawn add-wait thrds))))

  (defun add-wait (thr1 thr2)
    (add-simple (thread-value thr1) (thread-value thr2)))

  (defun add-simple (x1 x2)
    (format t "computing ~a + ~a ...\n" x1 x2)
    (binary+ x1 x2))

;;;-----------------------------------------------------------------------------
;;; Testing
;;;-----------------------------------------------------------------------------
  (defun test1 ()
    ;;((setter thread-concurrency) 16)
    (print (p-add 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))

  (defun make-list (x)
    (if (zerop x)
        ()
      (cons x (make-list (- x 1)))))

  (defun test2 ()
    (let (x)
      (while (progn
               (format t "Add (power of 2): ") (flush)
               (setq x (read lispin () (eos-default-value)))
               (null (eq x (eos-default-value))))
        ;;((setter thread-concurrency) x)
        (print (apply p-add (make-list x))))))
  (test2)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
