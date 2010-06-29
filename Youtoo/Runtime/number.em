;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: numbers
;;;-----------------------------------------------------------------------------
(defmodule number
  (syntax (_telos0)
   import (telos callback compare copy)
   export (<number> number? + - * / % gcd lcm mod
           abs zero? negate signum positive? negative?
           binary+ binary- binary* binary/ binary%
           binary-mod binary-gcd binary-lcm))

;;;-----------------------------------------------------------------------------
;;; Class <number>
;;;-----------------------------------------------------------------------------
  (defclass <number> (<object>) ()
    abstract?: t
    predicate: number?)

;;;-----------------------------------------------------------------------------
;;; Rest argument arithmetic
;;;-----------------------------------------------------------------------------
  (defun + args
    (labels
     ((loop (l res)
            (if (null? l) res
              (loop (cdr l) (binary+ (car l) res)))))
     (loop args 0)))

  (defun - (arg . args)
    (if (null? args)
        (binary- 0 arg)
      (labels
       ((loop (l res)
              (if (null? l) res
                (loop (cdr l) (binary- res (car l))))))
       (loop args arg))))

  (defun * args
    (labels
     ((loop (l res)
            (if (null? l) res
              (loop (cdr l) (binary* (car l) res)))))
     (loop args 1)))

  (defun / (arg . args)
    (labels
     ((loop (l res)
            (if (null? l) res
              (loop (cdr l) (binary/ res (car l))))))
     (loop args arg)))

  (defun % (arg . args)
    (labels
     ((loop (l res)
            (if (null? l) res
              (loop (cdr l) (binary% res (car l))))))
     (loop args arg)))

  (defun mod (arg . args)
    (labels
     ((loop (l res)
            (if (null? l) res
              (loop (cdr l) (binary-mod res (car l))))))
     (loop args arg)))

;;;-----------------------------------------------------------------------------
;;; Gcd, lcm
;;;-----------------------------------------------------------------------------
  (defun gcd (arg . args)
    (labels
     ((loop (l res)
            (if (null? l) res
              (loop (cdr l) (binary-gcd res (car l))))))
     (loop args arg)))

  (defun lcm (arg . args)
    (labels
     ((loop (l res)
            (if (null? l) res
              (loop (cdr l) (binary-lcm res (car l))))))
     (loop args arg)))

;;;-----------------------------------------------------------------------------
;;; Around zero ...
;;;-----------------------------------------------------------------------------
  ;; int-arithmetic traps to generic call if necessary
  (defun abs (x) (if (int-binary< x 0) (int-binary- 0 x) x))
  (declare-inline abs)

  (defun signum (x) (or (int-binary= x 0) (int-binary/ x (abs x))))
  (declare-inline signum)

  (defun positive? (x) (int-binary< 0 x))
  (declare-inline positive?)

  (defun negative? (x) (int-binary< x 0))
  (declare-inline negative?)

  (defgeneric zero? ((x <object>)))

  (defgeneric negate ((x <object>)))
  (defmethod negate ((x <number>)) (int-binary- 0 x))

;;;-----------------------------------------------------------------------------
;;; Generic arithmetic (also for collections ...)

;;;-----------------------------------------------------------------------------
  (defgeneric binary+ ((x <object>) (y <object>)))
  (defgeneric binary- ((x <object>) (y <object>)))
  (defgeneric binary* ((x <object>) (y <object>)))
  (defgeneric binary/ ((x <object>) (y <object>)))
  (defgeneric binary% ((x <object>) (y <object>)))
  (defgeneric binary-mod ((x <number>) (y <number>)))
  (defgeneric binary-gcd ((x <number>) (y <number>)))
  (defgeneric binary-lcm ((x <number>) (y <number>)))

;;;-----------------------------------------------------------------------------
;;; Install callback traps
;;;-----------------------------------------------------------------------------
  (install-callback (int-binary+ first-arithmetic-cb 0) binary+)
  (install-callback (int-binary+ first-arithmetic-cb 1) binary-)
  (install-callback (int-binary+ first-arithmetic-cb 2) binary*)
  (install-callback (int-binary+ first-arithmetic-cb 3) binary/)
  (install-callback (int-binary+ first-arithmetic-cb 4) binary%)

;;;-----------------------------------------------------------------------------
;;; Copying
;;;-----------------------------------------------------------------------------
  (defmethod shallow-copy ((x <number>)) x)
  (defmethod deep-copy ((x <number>)) x)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
