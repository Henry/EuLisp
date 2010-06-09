;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: tables; fixed comparator and hash-function;
;;;    assume non-relocating garbage collector!
;;;-----------------------------------------------------------------------------
(defmodule table
  (syntax (_telos0)
   import (telos convert copy collect compare list fpi string vector table1)
   expose (table1)
   export (table-ref table-empty?
           do1-table map1-table anyp1-table allp1-table))

;;;-----------------------------------------------------------------------------
;;; Table ref
;;;-----------------------------------------------------------------------------
  (defmethod element ((tab <table>) key)
    (table-ref tab key))

  (defgeneric table-ref (tab key))

  (defmethod table-ref ((tab <simple-hash-table>) key)
    (fast-table-ref tab (address-string key)))

  (defmethod table-ref ((tab <simple-hash-table>) (key <string>))
    ;; strings must have different key than symbols
    (fast-table-ref tab (string-append key "\x0001")))

  (defmethod table-ref ((tab <simple-hash-table>) (key <name>))
    (fast-table-ref tab (name key)))

  (defextern fast-table-ref (ptr <string>) ptr "eul_table_ref")
  (defextern address-string (ptr) <string> "eul_addr_str")

  (defmethod table-ref ((tab <hash-table>) key)
    (let ((entries (table-entries tab))
          (comp-fun (table-comparator tab))
          (hash-fun (table-hash-function tab)))
      (if (vectorp entries)
        (let* ((n (vector-size entries))
               (i (int-binary-mod (hash-fun key) n)))
          (labels
              ((loop (j)
                     (let ((entry (vector-ref entries j)))
                       (if (consp entry)
                           (if (comp-fun (car entry) key)
                               (cdr entry)
                             (let ((k (int-binary+ j 1)))
                               (if (int-binary< k n)
                                   (loop k)
                                 (loop 0))))
                         (table-fill-value tab)))))
            (loop i)))
        (table-fill-value tab))))

;;;-----------------------------------------------------------------------------
;;; Set table ref
;;;-----------------------------------------------------------------------------
  (defmethod (setter element) ((tab <table>) key x)
    ((setter table-ref) tab key x))

  (defgeneric (setter table-ref) (tab key x))

  (defmethod (setter table-ref) ((tab <simple-hash-table>) key x)
    (fast-table-set tab (address-string key) x))

  (defmethod (setter table-ref) ((tab <simple-hash-table>) (key <string>) x)
    ;; strings must have different key than symbols
    (fast-table-set tab (string-append key "\x0001") x))

  (defmethod (setter table-ref) ((tab <simple-hash-table>) (key <name>) x)
    (fast-table-set tab (name key) x))

  (defextern fast-table-set (ptr <string> ptr) ptr "eul_table_set")

  (defmethod (setter table-ref) ((tab <hash-table>) key x)
    (let ((entries (table-entries tab))
          (comp-fun (table-comparator tab))
          (hash-fun (table-hash-function tab)))
      (if (vectorp entries) ()
        ((setter table-entries) tab
         (setq entries (make-vector *min-table-entries*))))
      (let* ((n (vector-size entries))
             (i (int-binary-mod (hash-fun key) n)))
        (labels
            ((loop (j)
                   (let ((entry (vector-ref entries j)))
                     (if (consp entry)
                         (if (comp-fun (car entry) key)
                             (let ((old-value (cdr entry)))
                               ((setter cdr) entry x)
                               old-value)
                           (let ((k (int-binary+ j 1)))
                               (if (int-binary< k n)
                                   (loop k)
                                 (loop 0))))
                       (let* ((old-n (table-size tab))
                              (new-n (int-binary+ old-n 1)))
                         ((setter vector-ref) entries j (cons key x))
                         ((setter table-size) tab new-n)
                         (if (int-binary< new-n (table-threshold tab)) ()
                           (table-rehash tab))
                         (table-fill-value tab))))))
          (loop i)))))

;;;-----------------------------------------------------------------------------
;;; Simple table rehash
;;;-----------------------------------------------------------------------------
  (defun table-rehash (tab)
    (let ((entries (table-entries tab))
          (hash-fun (table-hash-function tab)))
      (if (vectorp entries)
          (let* ((old-n (vector-size entries))
                 (new-n (int-binary* old-n *table-fill-factor*))
                 (new-entries (make-vector new-n))
                 (old-t (table-threshold tab))
                 ;; retain relative fragmentation
                 (new-t (int-binary* old-t *table-fill-factor*)))
            ((setter table-entries) tab new-entries)
            ((setter table-threshold) tab new-t)
            (labels
                ((set-entry (i x)
                            (if (int-binary< i new-n)
                                (if (vector-ref new-entries i)
                                    (set-entry (int-binary+ i 1) x)
                                  ((setter vector-ref)
                                   new-entries i x))
                              (set-entry 0 x)))
                 (loop (j)
                       (if (int-binary< j old-n)
                           (let ((entry (vector-ref entries j)))
                             (if (consp entry)
                                 (let* ((key (car entry))
                                        (i (int-binary-mod
                                            (hash-fun key) new-n)))
                                   (set-entry i entry))
                               ())
                             (loop (int-binary+ j 1)))
                         tab)))
              (loop 0)))
          tab)))

;;;-----------------------------------------------------------------------------
;;; Predicates
;;;-----------------------------------------------------------------------------
  (defmethod emptyp ((tab <table>)) (table-empty? tab))

  (defun table-empty? (tab) (int-binary= (table-size tab) 0))
  ;;(declare-inline table-empty?)

;;;-----------------------------------------------------------------------------
;;; Do
;;;-----------------------------------------------------------------------------
  (defmethod do ((fun <function>) (tab <hash-table>) . cs)
    (if (null? cs)
        (do1-table fun tab)
      (error "do on multiple tables not yet implemented")))

  (defun do1-table (fun tab)
    ;; Attention -- key (ie (car entry) might not be a Lisp object when tab
    ;; is a simple hash table
    (do1-vector (lambda (entry)
                  (if (consp entry)
                      (fun (car entry) (cdr entry))
                    ()))
                (table-entries tab)))

;;;-----------------------------------------------------------------------------
;;; Map
;;;-----------------------------------------------------------------------------
  (defmethod map ((fun <function>) (tab <hash-table>) . cs)
    (if (null? cs)
        (map1-table fun tab)
      (error "map on multiple tables not yet implemented")))

  (defun map1-table (fun tab)
    ;; Attention -- key (ie (car entry) might not be a Lisp object when tab
    ;; is a simple hash table
    (let* ((entries (table-entries tab))
           (n (vector-size entries)))
      (if (vectorp entries)
          (labels
              ((loop (i res)
                     (if (int-binary< i n)
                         (let ((entry (vector-ref entries i))
                               (new-i (int-binary+ i 1)))
                           (if (consp entry)
                               (loop new-i
                                     (cons (fun (car entry) (cdr entry))
                                           res))
                             (loop new-i res)))
                       (reverse-list res))))
            (loop 0 ()))
        ())))

;;;-----------------------------------------------------------------------------
;;; Anyp
;;;-----------------------------------------------------------------------------
  (defmethod anyp ((fun <function>) (tab <hash-table>) . cs)
    (if (null? cs)
        (anyp1-table fun tab)
      (error "anyp on multiple tables not yet implemented")))

  (defun anyp1-table (fun tab)
    (anyp1-vector (lambda (entry)
                    (if (consp entry)
                        ()
                      (fun (car entry) (cdr entry))))
                  (table-entries tab)))

;;;-----------------------------------------------------------------------------
;;; Allp
;;;-----------------------------------------------------------------------------
  (defmethod allp ((fun <function>) (tab <hash-table>) . cs)
    (if (null? cs)
        (allp1-table fun tab)
      (error "allp on multiple tables not yet implemented")))

  (defun allp1-table (fun tab)
    (allp1-vector (lambda (entry)
                    (if (consp entry)
                        ()
                      (fun (car entry) (cdr entry))))
                  (table-entries tab)))

;;;-----------------------------------------------------------------------------
;;; Member
;;;-----------------------------------------------------------------------------
  (defmethod member (x (tab <table>) . preds)
    (if (null? preds)
        (table-ref tab x)
      (let ((fun (car preds)))
        (member x (table-entries tab)
                (lambda (entry)
                  (if (consp entry)
                      (fun x (car entry) (cdr entry))
                    ()))))))

;;;-----------------------------------------------------------------------------
;;; Accumulate
;;;-----------------------------------------------------------------------------
  (defmethod accumulate ((fun <function>) init (tab <table>))
    (accumulate-table fun init tab))

  (defun accumulate-table (fun init tab)
    (accumulate-list fun init (table-values tab)))

  (defmethod accumulate1 ((fun <function>) (tab <table>))
    (accumulate1-table fun tab))

  (defun accumulate1-table (fun tab)
    (accumulate1-list fun (table-values tab)))

;;;-----------------------------------------------------------------------------
;;; Conversion
;;;-----------------------------------------------------------------------------
  (defgeneric (converter <table>) (x))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
