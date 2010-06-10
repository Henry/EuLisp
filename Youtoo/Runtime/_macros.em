;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: level1 macros
;;;-----------------------------------------------------------------------------
(defmodule _macros
    (import (level1)
     expose (_telos0 _stream0)
     syntax (_boot0))

;;;-----------------------------------------------------------------------------
;;; Let/cc and unwind-protect
;;;-----------------------------------------------------------------------------
  (defmacro let/cc (k . body)
    ;; call/ep must not occur in tail position
    (let ((res (gensym)))
      `(let ((,res (call/ep (named-lambda call/ep-lambda (,k) ,@body))))
         ,res)))

  (defmacro unwind-protect (form . clean-up-forms)
    (let ((res-var (gensym)))
      `(progn
         (push-dynamic-variable
           '*clean-ups*
           (cons (lambda ()
                   ,@clean-up-forms
                   (pop-dynamic-variables 1))
                 (dynamic *clean-ups*)))
         (let ((,res-var ,form))
           ,@clean-up-forms
           (pop-dynamic-variables 1)
           ,res-var))))

;;;-----------------------------------------------------------------------------
;;; Block and return-from
;;;-----------------------------------------------------------------------------
  (defmacro block forms
    (cons 'let/cc forms))

  (defmacro return-from (name . forms)
    (list name (cons 'progn forms)))

;;;-----------------------------------------------------------------------------
;;; Catch and throw
;;;-----------------------------------------------------------------------------
  (defmacro catch (tag . body)
    (let ((k (gensym)))
      `(let/cc ,k
               (dynamic-let ((,tag ,k)) ,@body))))

  (defmacro throw (tag . forms)
    `((dynamic ,tag) (progn ,@forms)))

;;;-----------------------------------------------------------------------------
;;; Loopings
;;;-----------------------------------------------------------------------------
  (defmacro while (condition . body)
    (let ((loop (gensym)))
      `(let/cc break
               (labels
                 ((,loop ()
                         (when ,condition
                               ,@body
                               (,loop))))
                 (,loop)))))

  (defmacro for (init condition inc . body)
    `(progn ,init (while ,condition ,@body ,inc)))

;;;-----------------------------------------------------------------------------
;;; Define condition classes
;;;-----------------------------------------------------------------------------
  (defmacro defcondition (name super . init-options)
    (if (null? super)
        `(defclass ,name (<condition>) () ,@init-options)
      `(defclass ,name (,super) () ,@init-options)))

;;;-----------------------------------------------------------------------------
;;; Dynamic variables
;;; defglobal is top-level form only; must not be used within dynamic-let
;;;-----------------------------------------------------------------------------
  (defmacro defglobal (name val) `(push-dynamic-variable ',name ,val))

  (defmacro dynamic (name) `(dynamic-variable-ref ',name))

  (defmacro dynamic-setq (name val)
    `((setter dynamic-variable-ref) ',name ,val))

  (defmacro dynamic-let (name-vals . body)
    (let ((res (gensym)))
      `(progn
         ,@(map
             (lambda (name-val)
               `(push-dynamic-variable ',(car name-val) ,(car (cdr name-val))))
             name-vals)
         (unwind-protect (progn ,@body)
           (pop-dynamic-variables ,(size name-vals))))))

;;;-----------------------------------------------------------------------------
;;; Error handlers
;;;-----------------------------------------------------------------------------
  (defmacro with-handler (fun . body)
    (let ((res (gensym)))
      `(progn
         (push-error-handler ,fun)
         (let ((,res (progn ,@body)))
           (pop-error-handlers 1)
           ,res))))

;;;-----------------------------------------------------------------------------
;;; Auxiliary function (not necessarily EuLisp)
;;;-----------------------------------------------------------------------------
  (defmacro not (x) `(null? ,x))

  (defmacro butlast (list . number)
    (if (null? number)
        `(reverse-list (list-drop (reverse-list ,list) 1))
      `(reverse-list (list-drop (reverse-list ,list) ,@number))))

  (defmacro last (l . number)
    (if (null? number)
        `(list-drop ,l (- (list-size ,l) 1))
      `(list-drop ,l (- (list-size ,l) ,@number))))

  (defmacro time (expr . stream)
    (let ((x (gensym "time"))
          (res (gensym "time")))
      `(let ((,x (time-start))
             (,res ,expr))
         (time-stop ,x)
         (format ,(if stream (car stream) t)
                 "real: ~a\nuser: ~a\nsystem: ~a\n"
                 (vector-ref ,x 0) (vector-ref ,x 1) (vector-ref ,x 2))
         ,res)))

;;;-----------------------------------------------------------------------------
  )  ;; end of module
;;;-----------------------------------------------------------------------------
