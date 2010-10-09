;;; Copyright 1996 Andreas Kind
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
;;; Title: Benchmark which divides by 2 using lists of n ()'s.
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    Author:       Richard Gabriel
;;    Created:      8-Apr-85
;;    Modified:     19-Jul-85 18:28:01 (Bob Shaw)
;;                  23-Jul-87 (Will Clinger)
;;                  30-May-96 (Andreas Kind)
;;    This file contains a recursive as well as an iterative test.
;;;  Compilation:
;;    youtoo div -l level-0 -l math
;;;  Interpretation:
;;    (!> arith0)
;;;-----------------------------------------------------------------------------

(defmodule div
  (syntax (syntax-0)
   import (level-0 math)
   export (test-1
           test-2
           *ll*))

(defun create-n (n)
  (labels
   ((loop (m res)
          (if (< 0 m)
              (loop (- m 1) (cons () res))
            res)))
   (loop n ())))

(deflocal *ll* (create-n 200))

(defun iterative-div2 (l)
  (labels
   ((loop (ll a)
          (if (null? ll)
              a
            (loop (cddr ll) (cons (car ll) a)))))
   (loop l ())))

(defun recursive-div2 (l)
  (cond ((null? l) ())
        (t (cons (car l) (recursive-div2 (cddr l))))))

(defun test-1 (l)
  (labels
   ((loop (ll i)
          (if (< 0 i)
              (progn
                (iterative-div2 ll)
                (iterative-div2 ll)
                (iterative-div2 ll)
                (iterative-div2 ll)
                (loop ll (- i 1)))
            ())))
   (loop l 10000)))

(defun test-2 (l)
  (labels
   ((loop (ll i)
          (if (< 0 i)
              (progn
                (recursive-div2 ll)
                (recursive-div2 ll)
                (recursive-div2 ll)
                (recursive-div2 ll)
                (loop ll (- i 1)))
            ())))
   (loop l 10000)))

(defun run ()
  (test-1 *ll*)
  (test-2 *ll*))

(time-execution (run) stdout)

(print "Finished div" nl)

;;;-----------------------------------------------------------------------------
)  ;; End of module div
;;;-----------------------------------------------------------------------------
