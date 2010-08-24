;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         triang.em
; Description:  TRIANG -- Board game benchmark
; Author:       Richard Gabriel
; Created:      8-Apr-85
; Modified:     29-Oct-96 (Andreas Kind)
; Language:     EuLisp
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule triang
  (syntax (macros)
   import (level0 math))

(deflocal *answer* ())
(deflocal *final* ())
(deflocal *board*  (make <vector> size: 16 fill-value: 1))
(deflocal *sequence* (make <vector> size: 14 fill-value: 0))
(deflocal *a #(1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4 7
                 11 8 12 13 6 10 15 9 14 13 13 14 15 9 10 6 6))
(deflocal *b #(2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5 2
                 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5 5))
(deflocal *c #(4 7 11 8 12 13 6 10 15 9 14 13 13 14 15 9
                 10 6 1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4))
((setter vector-ref) *board* 5 0)

(defun last-position ()
  (labels
   ((loop (i)
          (cond ((= i (vector-size *board*)) 0)
                ((= 1 (vector-ref *board* i)) i)
                (t (loop (+ i 1))))))
   (loop 1)))

(defun try (i depth)
  (cond ((= depth 14)
         (let ((lp (last-position)))
           (unless (member lp *final*)
                   (setq *final* (cons lp *final*))))
         (setq *answer* (cons (cdr (convert *sequence* <list>)) *answer*))
         ;;(format "Answer: ~a~%" (car *answer*))
         t)
        ((and (= 1 (vector-ref *board* (vector-ref *a i)))
              (= 1 (vector-ref *board* (vector-ref *b i)))
              (= 0 (vector-ref *board* (vector-ref *c i))))
         ((setter vector-ref) *board* (vector-ref *a i) 0)
         ((setter vector-ref) *board* (vector-ref *b i) 0)
         ((setter vector-ref) *board* (vector-ref *c i) 1)
         ((setter vector-ref) *sequence* depth i)
         (labels
          ((loop (j d)
                 (if (or (= j 36) (try j d))
                     ()
                   (loop (+ j 1) d))))
          (loop 0 (+ depth 1)))
         ((setter vector-ref) *board* (vector-ref *a i) 1)
         ((setter vector-ref) *board* (vector-ref *b i) 1)
         ((setter vector-ref) *board* (vector-ref *c i) 0)
         ())
        (t ())))

(time-execution (try 22 1) stdout)
(print "triang finished")

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
