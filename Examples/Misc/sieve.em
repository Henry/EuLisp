;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Library: Misc
;;;  Authors: Andreas Kind
;;; Description: computing primes
;;;  Compilation
;;    youtoo sieve -l level-0
;;;-----------------------------------------------------------------------------
(defmodule sieve
  (syntax (macros)
   import (level-0))

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
)  ;; End of module
;;;-----------------------------------------------------------------------------
