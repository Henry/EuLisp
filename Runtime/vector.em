;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: level1 (EuLisp Language Level1 Implementation)
;;;  Authors: Andreas Kind, Julian Padget
;;;  Description: vectors
;;; -----------------------------------------------------------------------
(defmodule vector
  (syntax (_telos0)
   import (telos convert copy collect compare fpi list callback)
   export (<vector> vectorp make-vector vector-size
           maximum-vector-size
           vector-ref subvector vector-append vector-empty-p
           do1-vector map1-vector anyp1-vector allp1-vector
           reverse-vector member1-vector permute
           accumulate-vector accumulate1-vector reverse-vector!))
;;; --------------------------------------------------------------------
;;; Class <vector>
;;; --------------------------------------------------------------------
  (defprimclass <vector> vector-class (<sequence>) () predicate: vectorp)
  (defmethod initialize ((vec <vector>) inits)
    (call-next-method)
    (let ((n (init-list-ref inits size: 0))
          (x (init-list-ref inits fill-value:)))
      (make-vector1 n (list x))))
;  (defun make-vector (n . init)
;    (if init
;       (make-vector1 n init)
;      (primitive-allocate <vector> n)))
;;; --------------------------------------------------------------------
;;; Limit (2^29 - 1)
;;; --------------------------------------------------------------------
  (defconstant maximum-vector-size 536870911)
;;; --------------------------------------------------------------------
;;; Vector access
;;; --------------------------------------------------------------------
;  (defmethod size ((vec <vector>))
;    (vector-size vec))
  (defmethod size (x)
    (vector-size x))
  (defmethod element ((vec <vector>) (i <int>))
    (vector-ref vec i))
  (defmethod (setter element) ((vec <vector>) (i <int>) x)
    ((setter vector-ref) vec i x))
  (defun subvector (vec i j)
    (let* ((ii (or i 0))
           (jj (or j (vector-size vec)))
           (n (max (int-binary- jj ii) 0))
           (k ii)
           (res (make-vector n)))
      (labels
       ((loop ()
              (if (int-binary< k jj)
                  (progn
                    ((setter vector-ref) res (int-binary- k ii)
                     (vector-ref vec k))
                    (setq k (int-binary+ k 1))
                    (loop))
                res)))
       (loop))))
  (defmethod reset ((vec <vector>))
    (labels
     ((loop (i)
            (if (int-binary< i 0)
                vec
              (progn
                ((setter vector-ref) vec i ())
                (loop (int-binary- i 1))))))
       (loop (int-binary- (vector-size vec) 1))))
;;; --------------------------------------------------------------------
;;; Concatenation
;;; --------------------------------------------------------------------
  (defmethod concatenate ((vec1 <vector>) . cs)
    (labels
     ((loop (ccs)
            (if (null ccs) vec1
              (progn
                (setq vec1
                      (vector-append vec1 (convert (car ccs) <vector>)))
                (loop (cdr ccs))))))
     (loop cs)))
  (defun vector-append (vec1 vec2)
    (let* ((n1 (vector-size vec1))
           (n2 (vector-size vec2))
           (res (make-vector (int-binary+ n1 n2))))
      (labels
       ((loop (i j n vec)
              (if (int-binary< i n)
                  (progn
                    ((setter vector-ref)
                     res j (vector-ref vec i))
                    (loop (int-binary+ i 1) (int-binary+ j 1) n vec))
                res)))
       (loop 0 0 n1 vec1)
       (loop 0 n1 n2 vec2))))
;;; --------------------------------------------------------------------
;;; Predicates
;;; --------------------------------------------------------------------
  (defun vector-empty-p (vec) (int-binary= (vector-size vec) 0))
  (declare-inline vector-empty-p)
  (defmethod emptyp ((vec <vector>)) (vector-empty-p vec))
;;; --------------------------------------------------------------------
;;; General collection methods
;;; --------------------------------------------------------------------
  (defmethod anyp ((fun <function>) (c <collection>) . cs)
    (let* ((ccs (cons c cs))
           (n (apply min (map size ccs))))
      (labels
       ((loop (i)
              (and (int-binary< i n)
                   (or (apply fun (map (lambda (x) (element x i)) ccs))
                       (loop (int-binary+ i 1))))))
       (loop 0))))
  (defmethod allp ((fun <function>) (c <collection>) . cs)
    (let* ((ccs (cons c cs))
           (n (apply min (map size ccs))))
      (labels
       ((loop (i)
              (if (int-binary< i n)
                  (and (apply fun (map (lambda (x) (element x i)) ccs))
                       (loop (int-binary+ i 1)))
                t)))
       (loop 0))))
  (defmethod fill ((c <collection>) x . keys)
    (error "fill not yet implemented"))
;;; --------------------------------------------------------------------
;;; General do with arbitrary collections
;;; --------------------------------------------------------------------
  (defmethod do ((fun <function>) (c <collection>) . cs)
    (let* ((ccs (map1-list (lambda (cc)
                             (if (sequencep cc) cc (convert cc <vector>)))
                           (cons c cs)))
           (n (apply min (map1-list size ccs))))
      (labels
       ((loop (i)
              (if (int-binary< i n)
                  (progn
                    (apply fun (map1-list (lambda (cc)
                                            (element cc i)) ccs))
                    (loop (int-binary+ i 1)))
                ())))
       (loop 0))))
  (defmethod do ((fun <function>) (vec <vector>) . cs)
    (if (null cs)
        (do1-vector fun vec)
      (call-next-method)))
  (defun do1-vector (fun vec)
    (let ((n (vector-size vec)))
      (labels
       ((loop (i)
              (if (int-binary< i n)
                  (progn
                    (fun (vector-ref vec i))
                    (loop (int-binary+ i 1)))
                ())))
       (loop 0))))
;;; --------------------------------------------------------------------
;;; General map with arbitrary collections
;;; --------------------------------------------------------------------
  (defmethod map ((fun <function>) (c <collection>) . cs)
    (let* ((ccs (map1-list (lambda (cc)
                             (if (sequencep cc) cc (convert cc <vector>)))
                           (cons c cs)))
           (n (apply min (map1-list size ccs)))
           (res (make-vector n)))
      (labels
       ((loop (i)
              (if (int-binary< i n)
                  (progn
                    ((setter vector-ref)
                     res i
                     (apply fun (map1-list (lambda (cc)
                                             (element cc i)) ccs)))
                    (loop (int-binary+ i 1)))
                res)))
       (convert (loop 0) (class-of c)))))
  (defmethod map ((fun <function>) (vec <vector>) . cs)
    (if (null cs)
        (map1-vector fun vec)
      (call-next-method)))
  (defun map1-vector (fun vec)
    (let* ((n (vector-size vec))
           (res (make-vector n)))
      (labels
       ((loop (i)
              (if (int-binary< i n)
                  (progn
                    ((setter vector-ref) res i (fun (vector-ref vec i)))
                    (loop (int-binary+ i 1)))
                res)))
       (loop 0))))
;;; --------------------------------------------------------------------
;;; Member
;;; --------------------------------------------------------------------
  (defmethod member (x (vec <vector>) . preds)
    (if (null preds)
        (member1-vector x vec)
      (let ((pred (car preds))
            (n (vector-size vec)))
        (labels
         ((loop (i)
                (and (int-binary< i n)
                     (if (eql x (vector-ref vec i))
                         i
                       (loop (int-binary+ i 1))))))
         (loop 0)))))
  (defun member1-vector (x vec)
    (let ((n (vector-size vec)))
      (labels
       ((loop (i)
              (and (int-binary< i n)
                   (if (eql x (vector-ref vec i))
                       i
                     (loop (int-binary+ i 1))))))
       (loop 0))))
;;; --------------------------------------------------------------------
;;; Anyp
;;; --------------------------------------------------------------------
  (defmethod anyp ((fun <function>) (vec <vector>) . cs)
    (if (null cs)
        (anyp1-vector fun vec)
      (call-next-method)))
  (defun anyp1-vector (fun vec)
    (let ((n (vector-size vec)))
      (labels
       ((loop (i)
              (and (int-binary< i n)
                   (or (fun (vector-ref vec i))
                       (loop (int-binary+ i 1))))))
       (loop 0))))
;;; --------------------------------------------------------------------
;;; Allp
;;; --------------------------------------------------------------------
  (defmethod allp ((fun <function>) (vec <vector>) . cs)
    (if (null cs)
        (allp1-vector fun vec)
      (call-next-method)))
  (defun allp1-vector (fun vec)
    (let ((n (vector-size vec)))
      (labels
       ((loop (i)
              (if (int-binary< i n)
                  (and (fun (vector-ref vec i))
                       (loop (int-binary+ i 1)))
                t)))
       (loop 0))))
;;; --------------------------------------------------------------------
;;; Reverse
;;; --------------------------------------------------------------------
  (defmethod reverse ((vec <vector>))
    (reverse-vector vec))
  (defun reverse-vector (vec)
    (let* ((n (vector-size vec))
           (new-vec (make-vector n)))
      (labels
       ((loop (i j)
              (if (int-binary< i n)
                  (progn
                    ((setter vector-ref) new-vec i
                     (vector-ref vec j))
                    (loop (int-binary+ i 1)
                          (int-binary- j 1)))
                new-vec)))
       (loop 0 (int-binary- n 1)))))
  (defmethod reverse! ((vec <vector>))
    (reverse-vector! vec))
  (defun reverse-vector! (vec)
    ;; destructive reverse
    (labels
     ((loop (i j)
            (if (int-binary< i j)
                (let ((u (vector-ref vec i))
                      (v (vector-ref vec j)))
                  ((setter vector-ref) vec i v)
                  ((setter vector-ref) vec j u)
                  (loop (int-binary+ i 1)
                        (int-binary- j 1)))
              vec)))
     (loop 0 (int-binary- (vector-size vec) 1))))
;;; --------------------------------------------------------------------
;;; Accumulate
;;; --------------------------------------------------------------------
  (defmethod accumulate ((fun <function>) init (vec <vector>))
    (accumulate-vector fun init vec))
  (defun accumulate-vector (fun init vec)
    (let ((n (vector-size vec)))
      (labels
       ((loop (i)
              (if (int-binary< i n)
                  (progn
                    (setq init (fun init (vector-ref vec i)))
                    (loop (int-binary+ i 1)))
                init)))
       (loop 0))))
  (defmethod accumulate1 ((fun <function>) (vec <vector>))
    (accumulate1-vector fun vec))
  (defun accumulate1-vector (fun vec)
    (if (int-binary= (vector-size vec) 0) ()
      (accumulate-vector fun (vector-ref vec 0) (subvector vec 1 ()))))
;;; --------------------------------------------------------------------
;;; Permute collections
;;; --------------------------------------------------------------------
  (defun permute (fun . cs)
    (let (res)
      (labels
       ((loop (l args)
              (if (consp l)
                  (do
                   (lambda (x)
                     (loop (cdr l)
                           (cons x args)))
                   (car l))
                (setq res (cons (apply fun (reverse args)) res)))))
       (loop cs ())
       (if (or (null cs)
               (and (consp cs) (consp (car cs))))
           (reverse res)
         (convert (reverse res) (class-of (car cs)))))))
;;; --------------------------------------------------------------------
;;; Conversion and copying
;;; --------------------------------------------------------------------
  (defgeneric (converter <vector>) (x))
  (defmethod shallow-copy ((vec <vector>))
    (let* ((n (vector-size vec))
           (res (make <vector> size: n)))
      (labels
          ((loop (i)
                 (if (int-binary< i n)
                     (let ((x (vector-ref vec i)))
                       ((setter vector-ref) res i x)
                       (loop (int-binary+ i 1)))
                   res)))
        (loop 0))))
        
  (defmethod deep-copy ((vec <vector>))
    (let* ((n (vector-size vec))
           (res (make <vector> size: n)))
      (labels
          ((loop (i)
                 (if (int-binary< i n)
                     (let ((x (vector-ref vec i)))
                       ((setter vector-ref) res i (deep-copy x))
                       (loop (int-binary+ i 1)))
                   res)))
        (loop 0))))
)  ; end of module
