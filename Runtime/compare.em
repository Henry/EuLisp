;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: level1 (EuLisp Language Level1 Implementation)
;;;  Authors: Andreas Kind, Julian Padget
;;;  Description: comparison
;;; -----------------------------------------------------------------------
(defmodule compare
  (syntax (_telos0)
   import (telos callback)
   export (eq eql equal = binary= < <= binary< > >= max min))
;;; --------------------------------------------------------------------
;;; Equal
;;; --------------------------------------------------------------------
  (defgeneric equal (x y))
  (defmethod equal (x y) (eql x y))
;;; --------------------------------------------------------------------
;;; =
;;; --------------------------------------------------------------------
  (defun = (arg . args)
    (labels
     ((loop (l)
            (if (null l) t
              (if (null (binary= arg (car l))) ()
                (loop (cdr l))))))
     (loop args)))
  (defgeneric binary= (x y))
;;; --------------------------------------------------------------------
;;; < and <=
;;; --------------------------------------------------------------------
  (defun < (arg . args)
    (labels
     ((loop (l)
            (let ((rest (cdr l)))
              (if (null rest) t
                (if (null (binary< (car l) (car rest))) ()
                  (loop rest))))))
     (loop (cons arg args))))
  (defun <= (arg . args)
    (labels
     ((loop (l)
            (let ((rest (cdr l)))
              (if (null rest) t
                (if (let ((arg1 (car l))
                          (arg2 (car rest)))
                      (and (null (binary< arg1 arg2))
                           (null (binary= arg1 arg2))))
                    ()
                  (loop rest))))))
     (loop (cons arg args))))
  (defun > args (apply < (reverse-list args)))
  (defun >= args (apply <= (reverse-list args)))
  (defgeneric binary< (x y))
;;; --------------------------------------------------------------------
;;; Install callback traps
;;; --------------------------------------------------------------------
  (install-callback (int-binary+ first-arithmetic-cb 5) binary=)
  (install-callback (int-binary+ first-arithmetic-cb 6) binary<)
;;; --------------------------------------------------------------------
;;; Max and Moritz
;;; --------------------------------------------------------------------
  (defun max (arg . args)
    (labels
     ((loop (l res)
            (if (null l) res
              (let ((x (car l))
                    (ll (cdr l)))
                (if (binary< res x)
                    (loop ll x)
                  (loop ll res))))))
     (loop args arg)))
  (defun min (arg . args)
    (labels
     ((loop (l res)
            (if (null l) res
              (let ((x (car l))
                    (ll (cdr l)))
                (if (binary< res x)
                    (loop ll res)
                  (loop ll x))))))
     (loop args arg)))
)  ; end of module
