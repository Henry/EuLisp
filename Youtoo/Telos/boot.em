;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: boot module
;;;-----------------------------------------------------------------------------
(defmodule boot
  (syntax (_boot0)
   import (boot1)
   expose (boot1)
   export (append append! list-ref list-size
           map1-list do1-list member-list member1-list
           anyp1-list anyp2-list mapcan init-list-ref assoc-list-ref
           reverse-list sort-list list-remove list-remove-duplicates
           apply error *error* warning *warning*
           backtrace *backtrace-nframes* stack-values *stack-nvalues*
           getchar listify-env-string))

;;;-----------------------------------------------------------------------------
;;; List access
;;;-----------------------------------------------------------------------------
  (defun list-size (l)
    (labels
     ((loop (ll i)
            (if (atom? ll) i
              (loop (cdr ll) (+ i 1)))))
       (loop l 0)))

  (defun list-ref (l i)
    (if (= i 0)
        (car l)
      (list-ref (cdr l) (- i 1))))

  (defun (setter list-ref) (l i x)
    (labels
     ((loop (ll ii)
            (if (= ii 0)
                ((setter car) ll x)
              (loop (cdr ll) (- ii 1)))))
     (loop l i)))

  (defun reverse-list (l)
     (labels
      ((loop (ll res)
             (if (null? ll) res
               (loop (cdr ll) (cons (car ll) res)))))
      (loop l ())))

  (defun append (l1 l2)
    (labels
      ((loop (ll1 res)
             (if (null? ll1) res
               (loop (cdr ll1)
                     (cons (car ll1) res)))))
      (if (null? l2) l1
        (loop (reverse-list l1) l2))))

  (defun append! (l1 l2)  ;; Destructive
    (cond
      ((null? l1) l2)
      ((null? l2) l1)
      (t (labels
           ((loop (l rest)
                  (if rest
                      (loop rest (cdr rest))
                    ((setter cdr) l l2))))
           (loop l1 (cdr l1)))
         l1)))

;;;-----------------------------------------------------------------------------
;;; List mapping
;;;-----------------------------------------------------------------------------
  (defun member-list (x l . preds)
    (if (null? preds)
        (member1-list x l)
      (let ((pred (car preds)))
        (labels
         ((loop (ll)
                (if (null? ll) ()
                  (if (pred x (car ll))
                      ll
                    (loop (cdr ll))))))
         (loop l)))))

  (defun member1-list (x l)
    ((opencoded-lambda (xx ll default) (memq)) x l ()))
  (declare-inline member1-list)

  (defun do1-list (fun l)
    (labels
     ((loop (ll)
            (if (null? ll) ()
              (progn
                (fun (car ll))
                (loop (cdr ll))))))
     (loop l)))

  (defun map1-list (fun l)
    (labels
     ((loop (ll res)
            (if (null? ll) (reverse-list res)
              (loop (cdr ll) (cons (fun (car ll)) res)))))
     (loop l ())))

  (defun anyp1-list (fun l)
    (labels
     ((loop (ll)
            (if (atom? ll)
                (if (null? ll) ()
                  (fun ll))
              (or (fun (car ll))
                  (loop (cdr ll))))))
     (loop l)))

  (defun anyp2-list (fun l1 l2)
    (labels
     ((loop (ll1 ll2)
            (and ll1 ll2
                 (or (fun (car ll1) (car ll2))
                     (loop (cdr ll1) (cdr ll2))))))
     (loop l1 l2)))

;;;-----------------------------------------------------------------------------
;;; Some CommonLisp functions
;;;-----------------------------------------------------------------------------
  (defun list-remove (x l . preds)
    (if (null? preds)
        (list-remove1 x l)
      (let ((pred (car preds)))
        (labels
         ((loop (ll)
                (cond
                 ((null? ll) ())
                 ((pred x (car ll)) (cdr ll))
                 (t (let ((rem (loop (cdr ll))))
                      (if (eq rem (cdr ll))
                          ll
                        (cons (car ll) rem)))))))
         (loop l)))))

  (defun list-remove1 (x l)
    (labels
     ((loop (ll)
            (cond
             ((null? ll) ())
             ((eq x (car ll)) (cdr ll))
             (t (let ((rem (loop (cdr ll))))
                  (if (eq rem (cdr ll))
                      ll
                    (cons (car ll) rem)))))))
     (loop l)))

  (defun list-remove-duplicates (l . preds)
    (if preds
        (let ((pred (car preds)))
          (labels
           ((loop (ll res)
                  (if (null? ll) (reverse-list res)
                    (let ((x (car ll)))
                      (if (member-list x res pred)
                          (loop (cdr ll) res)
                        (loop (cdr ll) (cons x res)))))))
           (loop l ())))
      (labels
       ((loop (ll res)
              (if (null? ll) (reverse-list res)
                (let ((x (car ll)))
                  (if (member1-list x res)
                      (loop (cdr ll) res)
                    (loop (cdr ll) (cons x res)))))))
       (loop l ()))))

  (defun sort-list (l pred)
    (if (null? l)
        ()
      (let ((res (list (car l))))
        (labels
         ((insert (x ll)
                  (let ((rest (cdr ll)))
                    (if (null? rest)
                        (if (null? (pred x (car ll)))
                            ((setter cdr) ll (list x))
                          (let ((first (car ll)))
                            ((setter car) ll x)
                            ((setter cdr) ll (cons first rest))))
                      (if (null? (pred x (car ll)))
                          (insert x rest)
                        (let ((first (car ll)))
                          ((setter car) ll x)
                          ((setter cdr) ll (cons first rest))))))))
         (do1-list (lambda (x) (insert x res)) (cdr l)))
        res)))

  (defun init-list-ref (l x . default)
    ((opencoded-lambda (xx ll default) (iniq))
     x l (if default (car default) ())))

  (defun assoc-list-ref (l x . default)
    ((opencoded-lambda (xx ll default) (assq))
     x l (if default (car default) ())))

  (defun mapcan (fun l)
    (labels
     ((loop (ll res)
            (if (null? ll) res
              (let ((x (fun (car ll))))
                (if x
                    (loop (cdr ll) (append res x))
                  (loop (cdr ll) res))))))
     (loop l ())))

;;;-----------------------------------------------------------------------------
;;; Apply
;;;-----------------------------------------------------------------------------
  (defun apply (fun . args)
    ((opencoded-lambda (fun args) (apply)) fun args))

;;;-----------------------------------------------------------------------------
;;; Error and warning (using simple format)
;;;-----------------------------------------------------------------------------
  (deflocal *error* ())

  (defun error (str . args)
    (if *error*
        (apply *error* str args)
      (progn
       (write-object "*** ERROR [level1]: " stderr)
       (apply format stderr str args)
       (write-object "\n" stderr)  ; #\\n stderr)
       (write-object "***    See Backtrace? (y/n) " stderr)
       (if (eq (getchar) #\y) (backtrace) ())
       (exit))))

  (defextern getchar () <character>)

  (deflocal *warning* ())

  (defun warning (str . args)
    (if *warning*
        (apply *warning* str args)
      (progn
        (write-object "*** WARNING [level1]: " stderr)
        (apply format stderr str args)
        (write-object "\n" stderr)))) ;#\\n stderr))))

;;;-----------------------------------------------------------------------------
;;; Listify environment string
;;;-----------------------------------------------------------------------------
  (defun listify-env-string (str)
    (labels
     ((loop (str res)
            (let ((i (member1-string #\: str)))
              (if (null? i)
                  (reverse-list (cons str res))
                (if (= i 0)
                    (loop (tailstring str 1) res)
                  (loop (tailstring str (+ i 1))
                        (cons (substring str 0 i) res)))))))
     (if (string? str)
         (loop str ())
       ())))

;;;-----------------------------------------------------------------------------
;;; Backtrace and value stack
;;;-----------------------------------------------------------------------------
  (defopencoded lambda-name (x) (static-fpi-byte-ref 0) (primitive-ref))
  (defopencoded value-stack-ref (i) (value-stack-ref))
  (defopencoded context-stack-ref (i) (context-stack-ref))

  (defconstant *frame-size* 4)
  (deflocal *backtrace-nframes* 100)
  (defconstant *skip-nframes* 0)

  (defun show-frames (i)
    (if (= i *backtrace-nframes*) t
      (let ((fun (context-stack-ref (* i *frame-size*))))
        (if fun
            (let ((name (lambda-name fun)))
              (format stderr "       ~a: ~a\n" (- i *skip-nframes*) name)
              (show-frames (+ i 1)))
          ()))))

  (defun backtrace ()
    (sprint stderr "*** BACKTRACE:")
    ((opencoded-lambda () (unflush-stacks)))
    (show-frames *skip-nframes*))

  (deflocal *stack-nvalues* 25)
  (defconstant *skip-nvalues* 0)

  (defun show-stack-values (i)
    (if (= i *stack-nvalues*) t
      (let ((val (value-stack-ref i)))
        (format stderr "       ~a: ~a\n" (- i *skip-nvalues*) val)
        (show-stack-values (+ i 1)))))

  (defun stack-values ()
    (sprint stderr "*** STACK VALUES:")
    ((opencoded-lambda () (unflush-stacks)))
    (show-stack-values *skip-nvalues*))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
