;;; Copyright 1997 A. Kind & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;;  Library: scheme
;;;  Authors: Andreas Kind, Pete Broadbery, Luc Moreau
;;; Description: Scheme synatx (IEEE Std 1178-1990) in EuLisp
;;;-----------------------------------------------------------------------------
(defmodule scheme0
  (import ((except (do) level1)))

;;;-----------------------------------------------------------------------------
;;; Defining forms
;;;-----------------------------------------------------------------------------
(defmacro define (var-args . body)
  (let ((funp (cons? var-args))
        (local-defs ()))
    (if (cons? body)
        (labels
         ((loop (l res)
                (if l
                    (let ((x (car l)))
                      (if (cons? x)
                          (if (eq (car x) 'define)
                              (let* ((var-args1 (cadr x))
                                     (body1 (cddr x))
                                     (funp1 (cons? var-args1)))
                                (if funp1
                                    (loop (cdr l)
                                          (cons `(,(car var-args1)
                                                  (named-lambda
                                                   ,(car var-args1)
                                                   ,(cdr var-args1)
                                                   ,@body1))
                                                res))
                                  (loop (cdr l)
                                        (cons `(,var-args1 ,@body1)
                                              res))))
                            (progn
                              (setq local-defs (reverse res))
                              l))
                        (progn
                          (setq local-defs (reverse res))
                          l)))
                  (progn
                    (setq local-defs (reverse res))
                    l))))
         (setq body (loop body ())))
      ())
    (if local-defs
        `(progn
           (define1 ,var-args
                    (let ,(map car local-defs)
                      ,@(map (lambda (x)
                               `(setq ,(car x) ,(cadr x)))
                             local-defs)
                      ,@body)))
      `(define1 ,var-args ,@body))))

(defmacro define1 (name+args . body)
  ;; sorry, no 'and' available
  (if (symbol? name+args)
      (if body
          (if (cons? (car body))
              (if (eq (car (car body)) 'lambda)
                  `(deflocal ,name+args
                     (named-lambda ,name+args ,@(cdr (car body))))
                `(deflocal ,name+args ,@body))
            `(deflocal ,name+args ,@body))
        `(deflocal ,name+args ,@body))
    `(defun ,(car name+args) ,(cdr name+args) ,@body)))
(defmacro define-syntax (name+args . body)
  `(defmacro ,(car name+args) ,(cdr name+args) ,@body))

;;;-----------------------------------------------------------------------------
;;; Control syntax
;;;-----------------------------------------------------------------------------
(defmacro cond body
  (if body
      (if (cdr (car body))
          `(if ,(car (car body))
               (progn ,@(cdr (car body)))
             (cond ,@(cdr body)))
        `(or ,(car (car body)) (cond ,@(cdr body))))
    ()))

(defmacro and body
  (if body
      (if (cdr body)
          `(if ,(car body)
               (and ,@(cdr body))
             ())
        (car body))
    t))

(defmacro or body
  (if body
      (if (cdr body)
          (let ((x (gensym)))
            `(let ((,x ,(car body)))
               (if ,x
                   ,x
                 (or ,@(cdr body)))))
        (car body))
    ()))

(defmacro case (exp . cases)
  `(let ((else t) (case-val ,exp))
     (cond ,@(map (lambda (x)
                    (if (and (symbol? (car x)) (eq (car x) 'else))
                        x
                      `((eq? (quote ,(caar x)) case-val) ,(cadr x))))
                  cases))))

;;;-----------------------------------------------------------------------------
;;; letrec
;;;-----------------------------------------------------------------------------
(defmacro letrec (binds . body)
  `(let ,(map
          (lambda (bind)
            `(,(car bind) '()))
          binds)
     ,@(map
        (lambda (bind)
          `((setter ,(car bind)) ,@(cdr bind)))
        binds)
     ,@body))

(defun filter (pred l)
  (if (null? l) ()
    (if (pred (car l))
        (cons (car l) (filter pred (cdr l)))
      (filter pred (cdr l)))))

;;;-----------------------------------------------------------------------------
;;; do
;;;-----------------------------------------------------------------------------
(defmacro do (binds condn . body)
  ;; Be aware there is a EuLisp do with different sematics!
  (let ((constant (filter (lambda (bind) (= (size bind) 2)) binds))
        (stepped (filter (lambda (bind) (= (size bind) 3)) binds)))
    `(let ,constant
       (let do-loop
         ,(map
           (lambda (bind) (list (car bind) (cadr bind)))
           stepped)
         (if ,(car condn) (progn ,@(cdr condn))
           (progn
             ,@body
             (do-loop
              ,@(map (lambda (bind) (caddr bind)) stepped))))))))

;;;-----------------------------------------------------------------------------
;;; begin, set!, delay, force, cons-stream
;;;-----------------------------------------------------------------------------
(defmacro begin p `(progn . ,p))
(defmacro set! (x y) `(setq ,x ,y))
(defmacro delay (exp) `(lambda () ,exp))
(defmacro force (exp) `(apply ,exp ()))
(defmacro cons-stream (a b) `(cons ,a (delay ,b)))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
