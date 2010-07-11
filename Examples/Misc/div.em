;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         div.em
; Description:  DIV benchmarks
; Author:       Richard Gabriel
; Created:      8-Apr-85
; Modified:     19-Jul-85 18:28:01 (Bob Shaw)
;               23-Jul-87 (Will Clinger)
;               30-May-96 (Andreas Kind)
; Language:     EuLisp
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIV2 -- Benchmark which divides by 2 using lists of n ()'s.
;; This file contains a recursive as well as an iterative test.
(defmodule div
  (syntax (macros)
   import (level0)
   export (test-1 test-2 *ll*))

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

  (print "Finished div")

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
