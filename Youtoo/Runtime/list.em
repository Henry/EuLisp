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
           null? cons? list? atom? cons list list-size
           list-ref init-list-ref assoc-list-ref member-alist list-drop
           car cdr
           caar cadr cdar cddr
           caddr cdadr cddar caadr cdaar cadar caaar cdddr
           caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
           cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
           do1-list map1-list do2-list map2-list select-list
           reverse-list member-list member1-list anyp1-list find1-list
           do1-list-last-special map1-list-last-special
           proper-list? as-proper-list slice-list))

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
             (if (and (cons? l1) (cons? l2))
                 (and (binary= (car l1) (car l2))
                      (loop (cdr l1) (cdr l2)))
               (if l1
                   (and l2 (binary= l1 l2))
                 (null? l2)))))
      (loop x y)))

  ;;(defmethod binary= ((x <cons>) (y <cons>))
  ;;  (and (binary= (car x) (car y)) (binary= (cdr x) (cdr y))))
  ;; This is covered by (defmethod binary= ((<list>) (<list>)))
  ;; and it is not clear if a specialised implementation is beneficial.

;;;-----------------------------------------------------------------------------
;;;  Is a proper list?
;;;-----------------------------------------------------------------------------
  (defun proper-list? (l)
    (if (atom? l)
        (null? l)
      (proper-list? (cdr l))))

;;;-----------------------------------------------------------------------------
;;; Iteration
;;;-----------------------------------------------------------------------------
  (defmethod do ((fun <function>) (l <list>) . ls)
    (if (null? ls)
        (do1-list fun l)
      (if (null? (cdr ls))
          (do2-list fun l (car ls))
        (call-next-method))))

  (defmethod map ((fun <function>) (l <list>) . ls)
    (if (null? ls)
        (map1-list fun l)
      (if (null? (cdr ls))
          (map2-list fun l (car ls))
        (call-next-method))))

  (defun do2-list (fun l1 l2)
    (labels
     ((loop (ll1 ll2)
            (if (or (null? ll1) (null? ll2))
                ()
              (progn
                (fun (car ll1) (car ll2))
                (loop (cdr ll1) (cdr ll2))))))
     (loop l1 l2)))

  (defun map2-list (fun l1 l2)
    (labels
     ((loop (ll1 ll2 res)
            (if (or (null? ll1) (null? ll2))
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
            (if (null? (cdr ll))
                (fun2 (car ll))
              (progn
                (fun1 (car ll))
                (loop (cdr ll))))))
     (and l (loop l) ())))

  (defun map1-list-last-special (fun1 fun2 l)
    (labels
     ((loop (ll res)
            (if (null? (cdr ll))
                (reverse-list (cons (fun2 (car ll)) res))
              (loop (cdr ll) (cons (fun1 (car ll)) res)))))
     (and l (loop l ()))))

;;;-----------------------------------------------------------------------------
;;; Access
;;;-----------------------------------------------------------------------------
  (defun caar (x)
    ((opencoded-lambda (y) (caar)) x))
  (declare-inline caar)
  (defun (setter caar) (l x)
    ((setter car) (car l) x))

  (defun cadr (x)
    ((opencoded-lambda (y) (cadr)) x))
  (declare-inline cadr)
  (defun (setter cadr) (l x)
    ((setter car) (cdr l) x))

  (defun cdar (x)
    ((opencoded-lambda (y) (cdar)) x))
  (declare-inline cdar)
  (defun (setter cdar) (l x)
    ((setter cdr) (car l) x))

  (defun cddr (x)
    ((opencoded-lambda (y) (cddr)) x))
  (declare-inline cddr)
  (defun (setter cddr) (l x)
    ((setter cdr) (cdr l) x))

  (defun caddr (x)
    ((opencoded-lambda (y) (caddr)) x))
  (declare-inline caddr)
  (defun (setter caddr) (l x)
    ((setter car) (cddr l) x))

  (defun cdadr (x) (cdr (cadr x)))
  (declare-inline cdadr)
  (defun (setter cdadr) (l x)
    ((setter cdr) (cadr l) x))

  (defun cddar (x) (cdr (cdar x)))
  (declare-inline cddar)
  (defun (setter cddar) (l x)
    ((setter cdr) (cdar l) x))

  (defun caadr (x) (car (cadr x)))
  (declare-inline caadr)
  (defun (setter caadr) (l x)
    ((setter car) (cadr l) x))

  (defun cdaar (x) (cdr (caar x)))
  (declare-inline cdaar)
  (defun (setter cdaar) (l x)
    ((setter cdr) (caar l) x))

  (defun cadar (x) (car (cdar x)))
  (declare-inline cadar)
  (defun (setter cadar) (l x)
    ((setter car) (cdar l) x))

  (defun caaar (x) (car (caar x)))
  (declare-inline caaar)
  (defun (setter caaar) (l x)
    ((setter car) (caar l) x))

  (defun cdddr (x) (cdr (cddr x)))
  (declare-inline cdddr)
  (defun (setter cdddr) (l x)
    ((setter cdr) (cddr l) x))

  (defun caaaar (x) (caar (caar x)))
  (declare-inline caaaar)
  (defun (setter caaaar) (l x)
    ((setter car) (caaar l) x))

  (defun caaadr (x) (caaar (cdr x)))
  (declare-inline caaadr)
  (defun (setter caaadr) (l x)
    ((setter car) (caadr l) x))

  (defun caadar (x) (caar (cdar x)))
  (declare-inline caadar)
  (defun (setter caadar) (l x)
    ((setter car) (cadar l) x))

  (defun caaddr (x) (caar (cddr x)))
  (declare-inline caaddr)
  (defun (setter caaddr) (l x)
    ((setter car) (caddr l) x))

  (defun cadaar (x) (cadr (caar x)))
  (declare-inline cadaar)
  (defun (setter cadaar) (l x)
    ((setter car) (cdaar l) x))

  (defun cadadr (x) (cadr (cadr x)))
  (declare-inline cadadr)
  (defun (setter cadadr) (l x)
    ((setter car) (cdadr l) x))

  (defun caddar (x) (cadr (cdar x)))
  (declare-inline caddar)
  (defun (setter caddar) (l x)
    ((setter car) (cddar l) x))

  (defun cadddr (x)
    ((opencoded-lambda (y) (cadddr)) x))
  (declare-inline cadddr)
  (defun (setter cadddr) (l x)
    ((setter car) (cdddr l) x))

  (defun cdaaar (x) (cdar (caar x)))
  (declare-inline cdaaar)
  (defun (setter cdaaar) (l x)
    ((setter cdr) (caaar l) x))

  (defun cdaadr (x) (cdar (cadr x)))
  (declare-inline cdaadr)
  (defun (setter cdaadr) (l x)
    ((setter cdr) (caadr l) x))

  (defun cdadar (x) (cdar (cdar x)))
  (declare-inline cdadar)
  (defun (setter cdadar) (l x)
    ((setter cdr) (cadar l) x))

  (defun cdaddr (x) (cdar (cddr x)))
  (declare-inline cdaddr)
  (defun (setter cdaddr) (l x)
    ((setter cdr) (caddr l) x))

  (defun cddaar (x) (cddr (caar x)))
  (declare-inline cddaar)
  (defun (setter cddaar) (l x)
    ((setter cdr) (cdaar l) x))

  (defun cddadr (x) (cddr (cadr x)))
  (declare-inline cddadr)
  (defun (setter cddadr) (l x)
    ((setter cdr) (cdadr l) x))

  (defun cdddar (x) (cddr (cdar x)))
  (declare-inline cdddar)
  (defun (setter cdddar) (l x)
    ((setter cdr) (cddar l) x))

  (defun cddddr (x) (cddr (cddr x)))
  (declare-inline cddddr)
  (defun (setter cddddr) (l x)
    ((setter cdr) (cdddr l) x))

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
;;; Searching
;;;-----------------------------------------------------------------------------
  (defmethod member (x (l <list>) . preds)
    (apply member-list x l preds))

  (defmethod find (x (l <list>) . preds)
    (if (null? preds)
        (find1-list x l)
      (let ((pred (car preds)))
        (labels
         ((loop (ll i)
                (if (atom? ll) ll
                  (if (pred x (car ll)) i
                    (loop (cdr ll) (int-binary+ i 1))))))
         (loop l 0)))))

  (defun find1-list (x l)
    (labels
     ((loop (ll i)
            (if (atom? ll) ll
              (if (eql x (car ll)) i
                (loop (cdr ll) (int-binary+ i 1))))))
     (loop l 0)))

  (defmethod any? ((fun <function>) (l <list>) . cs)
    (if (null? cs)
        (anyp1-list fun l)
      (call-next-method)))

  (defmethod all? ((fun <function>) (l <list>) . cs)
    (if (null? cs)
        (all?1-list fun l)
      (if (null? (cdr cs))
          (all?2-list fun l (convert (car cs) <list>))
        (call-next-method))))

  (defun all?1-list (fun l)
    (labels
     ((loop (ll)
            (if (atom? ll)
                (if (null? ll) t
                  (fun ll))
              (and (fun (car ll))
                   (loop (cdr ll))))))
     (loop l)))

  (defun all?2-list (fun l1 l2)
    (labels
     ((loop (ll1 ll2)
            (if (or (atom? ll1) (atom? ll2))
                (if (and (null? ll1) (null? ll2)) t
                  (and (atom? ll1) (atom? ll2)
                       (fun ll1 ll2)))
              (and (fun (car ll1) (car ll2))
                   (loop (cdr ll1) (cdr ll2))))))
     (loop l1 l2)))

  (defun member-alist (x l . preds)
    (if (or (null? preds) (eq (car preds) eq))
        (assoc-list-ref l x)
      (let ((pred (car preds)))
        (labels
          ((loop (ll)
                 (if (null? ll) ()
                   (if (pred x (caar ll))
                       (car ll)
                     (loop (cdr ll))))))
          (loop l)))))

;;;-----------------------------------------------------------------------------
;;; Select
;;;-----------------------------------------------------------------------------
  (defmethod select ((fun <function>) (c <list>) . cs)
    (if (null? cs)
        (select-list fun c)
      (call-next-method)))

  (defun select-list (pred l . args)
    (labels
     ((loop (ll res)
            (if (null? ll)
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
            (if (null? ll) l
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
            (if (atom? ll) init
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
    (if (cons? l)
        (let ((x (cdr l)))
          (if (and x (atom? x))
              ((setter cdr) l (cons (cdr l) ())) ; destructive!
            (as-proper-list (cdr l))))
      ())
    (if (or (cons? l) (null? l))
        l
      (list l)))

;;;-----------------------------------------------------------------------------
;;; Copying
;;;-----------------------------------------------------------------------------
  (defgeneric (converter <list>) (x))

  (defmethod shallow-copy ((l <list>))
    (labels
     ((rev (ll res)
           (if (null? ll) res
             (rev (cdr ll) (cons (car ll) res))))
      (loop (ll res)
            (if (cons? ll)
                (loop (cdr ll) (cons (car ll) res))
              (rev res ll))))
      (loop l ())))

  (defmethod deep-copy ((l <list>))
    (labels
     ((rev (ll res)
           (if (null? ll) res
             (rev (cdr ll) (cons (car ll) res))))
      (loop (ll res)
            (if (cons? ll)
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
      (if (null? preds)
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
      (if (null? preds)
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
      (if (null? preds)
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
