;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: Misc
;;;  Authors: Andreas Kind
;;; Description: computing primes
;;;  Compilation
;;    youtoo sieve -l level0
;;;-----------------------------------------------------------------------------
(defmodule sieve
  (syntax (macros)
   import (level0))

(defun interval-list (m n)
  (if (> m n)
      ()
    (cons m (interval-list (+ 1 m) n))))

(defun sieve (l)
  (labels
   ((remove-multiples (n l)
                      (if (null? l)
                          ()
                        (if (= (binary% (car l) n) 0)
                            (remove-multiples n (cdr l))
                          (cons (car l)
                                (remove-multiples n (cdr l)))))))
   (if (null? l)
       ()
     (cons (car l)
           (sieve (remove-multiples (car l) (cdr l)))))))

(defun primes<= (n)
  (sieve (interval-list 2 n)))

(print (primes<= 300))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
