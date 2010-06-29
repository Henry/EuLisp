;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: misc
;;;  Authors: Andreas Kind
;;; Description: Towers of Hanoi
;;;  Compilation
;;    youtoo hanoi -l level1
;;;  Interpretation: (!> hanoi)
;;;-----------------------------------------------------------------------------
(defmodule hanoi
  (syntax (macros)
   import (level1)
   export (hanoi))

;;;-----------------------------------------------------------------------------
;;; Tower definition
;;;-----------------------------------------------------------------------------
  (defconstant *max-tower-height* 10)

  (defclass <tower> ()
    ((id reader: tower-id keyword: id:)
     (blocks accessor: tower-blocks)))

  (defun build-tower (x n)
    (labels ((loop (i res)
                   (if (= i 0) res
                     (loop (- i 1) (cons i res)))))
      ((setter tower-blocks) x (loop n ()))
      x))

  (defmethod generic-prin ((x <tower>) (s <stream>))
    (sformat s "#<tower ~a: ~a>" (tower-id x) (tower-blocks x)))

;;;-----------------------------------------------------------------------------
;;; Access to tower blocks
;;;-----------------------------------------------------------------------------
  (defgeneric push (x y))

  (defmethod push ((x <tower>) (y <int>))
    (let ((blocks (tower-blocks x)))
      (if (or (null? blocks) (< y (car blocks)))
          ((setter tower-blocks) x (cons y blocks))
        (error "cannot push block of size ~a on tower ~a" y x))))

  (defgeneric pop (x))

  (defmethod pop ((x <tower>))
    (let ((blocks (tower-blocks x)))
      (if blocks
          (progn
            ((setter tower-blocks) x (cdr blocks))
            (car blocks))
        (error "cannot pop block from emtpy tower ~a" x))))

;;;-----------------------------------------------------------------------------
;;; Move n blocks from tower x1 to tower x2 using x3 as buffer
;;;-----------------------------------------------------------------------------
  (defgeneric move (n x1 x2 x3))

  (defmethod move ((n <int>) (x1 <tower>) (x2 <tower>) (x3 <tower>))
    (if (= n 1)
        (progn
          (push x2 (pop x1))
          (print x1) (print x2) (print x3) (newline))
      (progn
        (move (- n 1) x1 x3 x2)
        (move 1 x1 x2 x3)
        (move (- n 1) x3 x2 x1))))

;;;-----------------------------------------------------------------------------
;;; Initialize and run the 'Towers of Hanoi'
;;;-----------------------------------------------------------------------------
  (defun hanoi ()
    (let ((x1 (make <tower> id: 0))
          (x2 (make <tower> id: 1))
          (x3 (make <tower> id: 2)))
      (build-tower x1 *max-tower-height*)
      (build-tower x2 0)
      (build-tower x3 0)
      (print x1) (print x2) (print x3)
      (move *max-tower-height* x1 x2 x3)))

  (hanoi)

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
