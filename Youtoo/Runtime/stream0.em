;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Julian Padget, Andreas Kind
;;; Description: stream syntax functions
;;;-----------------------------------------------------------------------------
(defmodule stream0
  (syntax (boot0)
   import (level1))

;;;-----------------------------------------------------------------------------
;;; (match-let (expression default-initializers) form*)
;;;-----------------------------------------------------------------------------
;;;  Examples of use
;;
;;;   (match-let foo ((a 1)) bar)
;;    => (let ((a (if (null? foo) 1 (car foo)))) bar)
;;
;;;   (match-let foo ((a 1) (b 2)) bar)
;;    => (let ((a ()) (b ()))
;;        (progn (if (null? foo) (setq a 1)
;;                   (progn (setq a (car foo)) (setq foo (cdr foo))))
;;               (if (null? foo) (setq b 2)
;;                   (progn (setq b (car foo)) (setq foo (cdr foo)))))
;;        bar)
;;
;;;   (match-let '(foo bar) ((a 1) (b 2)) baz)
;;    => (let ((G00055 (foo bar)) (a ()) (b ()))
;;        (progn (if (null? G00055) (setq a 1)
;;                   (progn (setq a (car G00055)) (setq G00055 (cdr G00055))))
;;               (if (null? G00055) (setq b 2)
;;                   (progn (setq b (car G00055)) (setq G00055 (cdr G00055)))))
;;        bar)
;;;-----------------------------------------------------------------------------
(defmacro match-let (expression default-initializers . body)
  (if (eql 1 (size default-initializers))
      `(let ((,(caar default-initializers)
              (if (null? ,expression)
                  ,(cadr (car default-initializers))
                (car ,expression))))
         ,@body)
    (let* ((var (if (symbol? expression) expression (gensym)))
           (update-vars
            (labels
             ((loop (l)
                    (if (null? l)
                        ()
                      (cons
                       `(if (null? ,var)
                            (setq ,(caar l) ,(cadr (car l)))
                          (progn
                            (setq ,(caar l) (car ,var))
                            (setq ,var (cdr ,var))))
                       (loop (cdr l))))))
             (loop default-initializers))))
      `(let (,@(if (symbol? expression) () `((,var ,expression)))
             ,@(map (lambda (x) `(,(car x) ())) default-initializers))
         (progn ,@update-vars)
         ,@body))))

;;;-----------------------------------------------------------------------------
;;; (with-lock (lock-valued-expression) form*)
;;;-----------------------------------------------------------------------------
(defmacro with-lock (lock . body)
  (let ((the-lock (gensym)))
    `(let ((,the-lock ,lock))
       (unwind-protect
           (progn (lock ,the-lock) ,@body)
         (unlock ,the-lock)))))

;;;-----------------------------------------------------------------------------
;;; (with-source (identifier expression) form*)
;;;  Temporarily reconnects source of identifier to expression
;;;-----------------------------------------------------------------------------
(defmacro with-source (decl . body)
  (let ((the-source (gensym)))
    `(let ((,the-source (source ,(car decl))))
       (reconnect ,(car (cdr decl)) ,(car decl))
       (unwind-protect
           (progn ,@body)
         (reconnect ,the-source ,(car decl))))))

;;;-----------------------------------------------------------------------------
;;; (with-source (identifier expression) form*)
;;;  Temporarily reconnects sink of identifier to expression
;;;-----------------------------------------------------------------------------
(defmacro with-sink (decl . body)
  (let ((the-source (gensym)))
    `(let ((,the-source (source ,(car decl))))
       (reconnect ,(car (cdr decl)) ,(car decl))
       (unwind-protect
           (progn ,@body)
         (reconnect ,the-source ,(car decl))))))

;;;-----------------------------------------------------------------------------
;;;  (with-input-file (var-and-file-name)  form*)
;;;  Temporarily binds var with input stream
;;;-----------------------------------------------------------------------------
(defmacro with-input-file (var-and-file-name . body)
  (let ((s (car var-and-file-name))
        (file-name (car (cdr var-and-file-name)))
        (res (gensym)))
    `(let ((,s (make <file-stream> file-name: ,file-name))
           (,res ()))
       (unwind-protect (setq ,res (progn ,@body))
         (disconnect ,s))
       ,res)))

;;;-----------------------------------------------------------------------------
;;;  (with-output-file (var-and-file-name)  form*)
;;;  Temporarily connects stdout to file-name
;;;-----------------------------------------------------------------------------
(defmacro with-output-file (var-and-file-name . body)
  (let ((s (car var-and-file-name))
        (file-name (car (cdr var-and-file-name)))
        (res (gensym)))
    `(let ((,s (make <file-stream> file-name: ,file-name mode: 'w))
           (,res ()))
       (unwind-protect (setq ,res (progn ,@body))
         (disconnect ,s))
       ,res)))

;;;-----------------------------------------------------------------------------
;;; (with-input-file-of-path (decl) form*)
;;;  Open input file with path
;;;-----------------------------------------------------------------------------
(defmacro with-input-file-of-path (decl . body)
  (let ((s (car decl))
        (name (car (cdr decl)))
        (dir (car (cdr (cdr decl))))
        (path (car (cdr (cdr (cdr decl)))))
        (info (gensym))
        (file-name (gensym))
        (res (gensym)))
    `(let ((,info (apply file-lookup ,name ,path)))
       (if (null? ,info)
           (error <condition>
                   (fmt "No such file or directory ~a in ~a" ,name ,path))
         (let ((,file-name (car ,info))
               (,dir (cdr ,info)))
           (with-input-file (,s ,file-name) ,@body))))))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
