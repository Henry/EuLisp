;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: list processing
;;;-----------------------------------------------------------------------------
(defmodule list
  (syntax (_telos0)
   import (telos convert compare collect copy number fpi)
   export (<null> <cons> <list>
           accumulate-list accumulate1-list
           null car cdr consp listp atom cons list list-size
           list-ref init-list-ref assoc-list-ref list-drop
           caar cadr cdar cddr
           caddr cdadr cddar caadr cdaar cadar caaar cdddr cadddr
           do1-list map1-list do2-list map2-list select-list
           reverse-list member-list member1-list anyp1-list find1-list
           do1-list-last-special map1-list-last-special
           proper-list? as-proper-list reverse-list! slice-list))

;;;-----------------------------------------------------------------------------
;;; Classes: <list>, <cons> and <null>
;;    Defined in mop-class
;;    But superclass <sequence> wasn't defined at that stage
;;;-----------------------------------------------------------------------------
  (add-subclass <sequence> <list>)

  (let ((direct-supers (list <sequence>)))
    ((setter class-direct-superclasses) <list> direct-supers)
    ((setter class-precedence-list) <list>
     (compute-class-precedence-list <list> direct-supers)))

  (let ((direct-supers (list <list>)))
    ((setter class-precedence-list) <cons>
     (compute-class-precedence-list <cons> direct-supers))
    ((setter class-precedence-list) <null>
     (compute-class-precedence-list <null> direct-supers)))

;;;-----------------------------------------------------------------------------
;;; Predicates
;;;-----------------------------------------------------------------------------

  (defmethod binary= ((x <list>) (y <list>))
    (labels
      ((loop (l1 l2)
             (if (and (consp l1) (consp l2))
                 (and (binary= (car l1) (car l2))
                      (loop (cdr l1) (cdr l2)))
               (if l1
                   (and l2 (binary= l1 l2))
                 (null l2)))))
      (loop x y)))

  ;;(defmethod binary= ((x <cons>) (y <cons>))
  ;;  (and (binary= (car x) (car y)) (binary= (cdr x) (cdr y))))
  ;; This is covered by (defmethod binary= ((<list>) (<list>)))
  ;; and it is not clear if a specialised implementation is beneficial.

;;;-----------------------------------------------------------------------------
;;;  Is a proper list?
;;;-----------------------------------------------------------------------------
  (defun proper-list? (l)
    (if (atom l)
        (null l)
      (proper-list? (cdr l))))

;;;-----------------------------------------------------------------------------
;;; Iteration
;;;-----------------------------------------------------------------------------
  (defmethod do ((fun <function>) (l <list>) . ls)
    (if (null ls)
        (do1-list fun l)
      (if (null (cdr ls))
          (do2-list fun l (car ls))
        (call-next-method))))

  (defmethod map ((fun <function>) (l <list>) . ls)
    (if (null ls)
        (map1-list fun l)
      (if (null (cdr ls))
          (map2-list fun l (car ls))
        (call-next-method))))

  (defun do2-list (fun l1 l2)
    (labels
     ((loop (ll1 ll2)
            (if (or (null ll1) (null ll2))
                ()
              (progn
                (fun (car ll1) (car ll2))
                (loop (cdr ll1) (cdr ll2))))))
     (loop l1 l2)))

  (defun map2-list (fun l1 l2)
    (labels
     ((loop (ll1 ll2 res)
            (if (or (null ll1) (null ll2))
                (reverse-list res)
              (loop (cdr ll1) (cdr ll2)
                    (cons (fun (car ll1) (car ll2)) res)))))
     (loop l1 l2 ())))

;;;-----------------------------------------------------------------------------
;;;  Handle the last element in a list specially
;;;-----------------------------------------------------------------------------
  (defun do1-list-last-special (fun1 fun2 l)
    (labels
     ((loop (ll)
            (if (null (cdr ll))
                (fun2 (car ll))
              (progn
                (fun1 (car ll))
                (loop (cdr ll))))))
     (and l (loop l) ())))

  (defun map1-list-last-special (fun1 fun2 l)
    (labels
     ((loop (ll res)
            (if (null (cdr ll))
                (reverse-list (cons (fun2 (car ll)) res))
              (loop (cdr ll) (cons (fun1 (car ll)) res)))))
     (and l (loop l ()))))

;;;-----------------------------------------------------------------------------
;;; Access
;;;-----------------------------------------------------------------------------
  (defun caar (x) ((opencoded-lambda (y) (caar)) x))
  (declare-inline caar)

  (defun cadr (x) ((opencoded-lambda (y) (cadr)) x))
  (declare-inline cadr)

  (defun cdar (x) ((opencoded-lambda (y) (cdar)) x))
  (declare-inline cdar)

  (defun cddr (x) ((opencoded-lambda (y) (cddr)) x))
  (declare-inline cddr)

  (defun caddr (x) ((opencoded-lambda (y) (caddr)) x))
  (declare-inline caddr)

  (defun cdadr (x) (cdr (cadr x)))
  (declare-inline cdadr)

  (defun cddar (x) (cdr (cdar x)))
  (declare-inline cddar)

  (defun caadr (x) (car (cadr x)))
  (declare-inline caadr)

  (defun cdaar (x) (cdr (caar x)))
  (declare-inline cdaar)

  (defun cadar (x) (car (cdar x)))
  (declare-inline cadar)

  (defun caaar (x) (car (caar x)))
  (declare-inline caaar)

  (defun cdddr (x) (cdr (cddr x)))
  (declare-inline cdddr)

  (defun cadddr (x) ((opencoded-lambda (y) (cadddr)) x))
  (declare-inline cadddr)

  (defmethod element ((l <list>) (i <int>))
    (list-ref l i))

  (defmethod (setter element) ((l <list>) (i <int>) x)
    ((setter list-ref) l i x))

  (defun list-drop (l n)
    (if (int-binary= n 0) l
      (list-drop (cdr l) (int-binary- n 1))))

;;;-----------------------------------------------------------------------------
;;; Length
;;;-----------------------------------------------------------------------------
  (defmethod size ((l <list>))
    (list-size l))

;;;-----------------------------------------------------------------------------
;;; Checking
;;;-----------------------------------------------------------------------------
  (defmethod member (x (l <list>) . preds)
    (apply member-list x l preds))

  (defmethod find (x (l <list>) . preds)
    (if (null preds)
        (find1-list x l)
      (let ((pred (car preds)))
        (labels
         ((loop (ll i)
                (if (atom ll) ll
                  (if (pred x (car ll)) i
                    (loop (cdr ll) (int-binary+ i 1))))))
         (loop l 0)))))

  (defun find1-list (x l)
    (labels
     ((loop (ll i)
            (if (atom ll) ll
              (if (eql x (car ll)) i
                (loop (cdr ll) (int-binary+ i 1))))))
     (loop l 0)))

  (defmethod anyp ((fun <function>) (l <list>) . cs)
    (if (null cs)
        (anyp1-list fun l)
      (call-next-method)))

  (defmethod allp ((fun <function>) (l <list>) . cs)
    (if (null cs)
        (allp1-list fun l)
      (if (null (cdr cs))
          (allp2-list fun l (convert (car cs) <list>))
        (call-next-method))))

  (defun allp1-list (fun l)
    (labels
     ((loop (ll)
            (if (atom ll)
                (if (null ll) t
                  (fun ll))
              (and (fun (car ll))
                   (loop (cdr ll))))))
     (loop l)))

  (defun allp2-list (fun l1 l2)
    (labels
     ((loop (ll1 ll2)
            (if (or (atom ll1) (atom ll2))
                (if (and (null ll1) (null ll2)) t
                  (and (atom ll1) (atom ll2)
                       (fun ll1 ll2)))
              (and (fun (car ll1) (car ll2))
                   (loop (cdr ll1) (cdr ll2))))))
     (loop l1 l2)))

;;;-----------------------------------------------------------------------------
;;; Select
;;;-----------------------------------------------------------------------------
  (defmethod select ((fun <function>) (c <list>) . cs)
    (if (null cs)
        (select-list fun c)
      (call-next-method)))

  (defun select-list (pred l . args)
    (labels
     ((loop (ll res)
            (if (null ll)
                (reverse-list res)
              (let ((x (car ll)))
                (if (apply pred x args)
                    (loop (cdr ll) (cons x res))
                  (loop (cdr ll) res))))))
     (loop l ())))

;;;-----------------------------------------------------------------------------
;;; Reverse
;;;-----------------------------------------------------------------------------
  (defmethod reverse ((l <list>))
    (reverse-list l))

  (defun reverse-list! (l)
    (labels
     ((loop (ll res)
            (if (null ll) res
              (let ((u (cdr ll)))
                ((setter cdr) ll res)
                (loop u ll)))))
     (loop l ())))

  (defmethod reverse! ((l <list>))
    ;; destructive
    (reverse-list! l))

;;;-----------------------------------------------------------------------------
;;; Sort
;;;-----------------------------------------------------------------------------
  (defmethod sort ((l <list>) . comp)
    (sort-list l (if comp (car comp) binary<)))

  (defmethod remove ((l <list>) x . pred)
    (apply list-remove l x pred))

;;;-----------------------------------------------------------------------------
;;; Concatenate
;;;-----------------------------------------------------------------------------
  (defmethod concatenate ((l <list>) . cs)
    (labels
     ((loop (ll)
            (if (null ll) l
              (progn
                (setq l (append l (convert (car ll) <list>)))
                (loop (cdr ll))))))
     (loop cs)))

;;;-----------------------------------------------------------------------------
;;; Slice
;;;-----------------------------------------------------------------------------
  (defun slice-list (list from to)
    (if (>= from to) ()
      (cons (element list from) (slice-list list (+ from 1) to))))

  (defmethod slice ((list <list>) (s <int>) (e <int>))
    (slice-list list s e))

;;;-----------------------------------------------------------------------------
;;; Accumulate
;;;-----------------------------------------------------------------------------
  (defmethod accumulate ((fun <function>) init (l <list>))
    (accumulate-list fun init l))

  (defun accumulate-list (fun init l)
    (labels
     ((loop (ll)
            (if (atom ll) init
              (progn
                (setq init (fun init (car ll)))
                (loop (cdr ll))))))
     (loop l)))

  (defmethod accumulate1 ((fun <function>) (l <list>))
    (accumulate1-list fun l))

  (defun accumulate1-list (fun l)
    (and l (accumulate-list fun (car l) (cdr l))))

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
  (defgeneric (converter <list>) (x))

;;;-----------------------------------------------------------------------------
;;;  Convert to a proper list
;;;-----------------------------------------------------------------------------
  (defun as-proper-list (l)
    (if (consp l)
        (let ((x (cdr l)))
          (if (and x (atom x))
              ((setter cdr) l (cons (cdr l) ())) ; destructive!
            (as-proper-list (cdr l))))
      ())
    (if (or (consp l) (null l))
        l
      (list l)))

;;;-----------------------------------------------------------------------------
;;; Copying
;;;-----------------------------------------------------------------------------
  (defgeneric (converter <list>) (x))

  (defmethod shallow-copy ((l <list>))
    (labels
     ((rev (ll res)
           (if (null ll) res
             (rev (cdr ll) (cons (car ll) res))))
      (loop (ll res)
            (if (consp ll)
                (loop (cdr ll) (cons (car ll) res))
              (rev res ll))))
      (loop l ())))

  (defmethod deep-copy ((l <list>))
    (labels
     ((rev (ll res)
           (if (null ll) res
             (rev (cdr ll) (cons (car ll) res))))
      (loop (ll res)
            (if (consp ll)
                (loop (cdr ll) (cons (deep-copy (car ll)) res))
              (if ll
                  (rev res (deep-copy ll))
                (rev res ())))))
      (loop l ())))

  ;(defmethod deep-copy ((l <cons>))
  ;  (cons (deep-copy (car l)) (deep-copy (cdr l))))

;;;-----------------------------------------------------------------------------
;;; Arithmetic
;;    Arity does not correspond to the EuLisp definition!
;;    This is used internally but unfortunately exposed at the moment.
;;;-----------------------------------------------------------------------------
  (defmethod binary+ ((l1 <list>) (l2 <list>) . preds)
    ;; list union
    (let ((res ()))
      (if (null preds)
          (do1-list (lambda (x)
                      (if (member1-list x l1) ()
                        (setq res (cons x res))))
                    l2)
        (do1-list (lambda (x)
                    (if (member-list x l1 (car preds)) ()
                      (setq res (cons x res))))
                  l2))
      (append l1 res)))

  (defmethod binary- ((l1 <list>) (l2 <list>) . preds)
    ;; list difference
    (let ((res ()))
      (if (null preds)
          (do1-list (lambda (x)
                      (if (member1-list x l2) ()
                        (setq res (cons x res))))
                    l1)
        (do1-list (lambda (x)
                    (if (member-list x l2 (car preds)) ()
                      (setq res (cons x res))))
                  l1))
      res))

  (defmethod binary/ ((l1 <list>) (l2 <list>) . preds)
    ;; intersection
    (let ((res ()))
      (if (null preds)
          (do1-list (lambda (x)
                      (if (member1-list x l1)
                          (setq res (cons x res))
                        ()))
                    l2)
        (do1-list (lambda (x)
                    (if (member-list x l1 (car preds))
                        (setq res (cons x res))
                      ()))
                  l2))
      res))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
