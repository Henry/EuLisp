;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         puzzle.em
; Description:  PUZZLE benchmark
; Author:       Richard Gabriel, after Forrest Baskett
; Created:      12-Apr-85
; Modified:     12-Apr-85 14:20:23 (Bob Shaw)
;               11-Aug-87 (Will Clinger)
;               22-Jan-88 (Will Clinger)
;               30-May-96 (Andreas Kind)
; Language:     EuLisp
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmodule puzzle
  (syntax (macros scheme0)
   import (level1))

;; PUZZLE -- Forest Baskett's Puzzle benchmark, originally written in Pascal.
(defconstant *size* 511)
(defconstant *classmax* 3)
(defconstant *typemax* 12)

(deflocal *iii* 0)
(deflocal *kount* 0)
(deflocal *d* 8)
(deflocal *piececount* (make <vector> size: (+ *classmax* 1) fill-value: 0))
(deflocal *class* (make <vector> size: (+ *typemax* 1) fill-value: 0))
(deflocal *piecemax* (make <vector> size: (+ *typemax* 1) fill-value: 0))
(deflocal *puzzle* (make <vector> size: (+ *size* 1)))
(deflocal *p* (make <vector> size: (+ *typemax* 1)))

(labels
 ((loop (i)
        (cond
          ((null? (> i *typemax*))
           ((setter vector-ref) *p* i (make <vector> size: (+ *size* 1)))
           (loop (+ i 1))))))
 (loop 0))

(defun fit (i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((or (> k end)
             (and (vector-ref (vector-ref *p* i) k)
                  (vector-ref *puzzle* (+ j k))))
         (if (> k end) t ())))))

(defun place (i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
        (cond ((vector-ref (vector-ref *p* i) k)
               ((setter vector-ref) *puzzle* (+ j k) t)
               t)))
    ((setter vector-ref) *piececount*
     (vector-ref *class* i)
     (- (vector-ref *piececount* (vector-ref *class* i)) 1))
    (do ((k j (+ k 1)))
        ((or (> k *size*) (null? (vector-ref *puzzle* k)))
         (if (> k *size*) 0 k)))))

(defun puzzle-remove (i j)
  (let ((end (vector-ref *piecemax* i)))
    (do ((k 0 (+ k 1)))
        ((> k end))
        (cond ((vector-ref (vector-ref *p* i) k)
               ((setter vector-ref) *puzzle* (+ j k) ())
               ())))
    ((setter vector-ref) *piececount*
     (vector-ref *class* i)
     (+ (vector-ref *piececount* (vector-ref *class* i)) 1))))

(defun trial (j)
  (let/cc kk
    (let ((k 0))
      (do ((i 0 (+ i 1)))
          ((> i *typemax*) (setq *kount* (+ *kount* 1)))
          (cond
            ((null?
              (zero?
               (vector-ref *piececount* (vector-ref *class* i))))
             (cond
               ((fit i j)
                (setq k (place i j))
                (cond
                  ((or (trial k) (zero? k))
                   (format "Piece ~a at ~a.\n" (+ i 1) (+ k 1))
                   (setq *kount* (+ *kount* 1))
                   (kk t))
                  (t (puzzle-remove i j)))))))))))

(defun definepiece (iclass ii jj kk)
  (let ((index 0))
    (do ((i 0 (+ i 1)))
        ((> i ii))
        (do ((j 0 (+ j 1)))
            ((> j jj))
            (do ((k 0 (+ k 1)))
                ((> k kk))
                (setq index (+ i (* *d* (+ j (* *d* k)))))
                ((setter vector-ref) (vector-ref *p* *iii*) index  t))))
    ((setter vector-ref) *class* *iii* iclass)
    ((setter vector-ref) *piecemax* *iii* index)
    (cond ((null? (= *iii* *typemax*))
           (setq *iii* (+ *iii* 1))))))

(defun start ()
  (do ((m 0 (+ m 1)))
      ((> m *size*))
      ((setter vector-ref) *puzzle* m t))
  (do ((i 1 (+ i 1)))
      ((> i 5))
      (do ((j 1 (+ j 1)))
          ((> j 5))
          (do ((k 1 (+ k 1)))
              ((> k 5))
              ((setter vector-ref) *puzzle* (+ i (* *d* (+ j (* *d* k)))) ()))))
  (do ((i 0 (+ i 1)))
      ((> i *typemax*))
      (do ((m 0 (+ m 1)))
          ((> m *size*))
          ((setter vector-ref) (vector-ref *p* i) m ())))
  (setq *iii* 0)

  (definepiece 0 3 1 0)
  (definepiece 0 1 0 3)
  (definepiece 0 0 3 1)
  (definepiece 0 1 3 0)
  (definepiece 0 3 0 1)
  (definepiece 0 0 1 3)
  (definepiece 1 2 0 0)
  (definepiece 1 0 2 0)
  (definepiece 1 0 0 2)
  (definepiece 2 1 1 0)
  (definepiece 2 1 0 1)
  (definepiece 2 0 1 1)
  (definepiece 3 1 1 1)

  ((setter vector-ref) *piececount* 0 13)
  ((setter vector-ref) *piececount* 1 3)
  ((setter vector-ref) *piececount* 2 1)
  ((setter vector-ref) *piececount* 3 1)

  (let ((m (+ (* *d* (+ *d* 1)) 1))
        (n 0))
    (cond ((fit 0 m)
           (setq n (place 0 m)))
          (t
           (print "Error.")))
    (cond ((trial n)
           (format "Success in ~a trials.\n" *kount*))
          (t
           (print "Failure.\n")))))

(start)

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
