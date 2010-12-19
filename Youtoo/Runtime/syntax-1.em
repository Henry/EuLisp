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
;;; Title: level-1 syntax
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule syntax-1
  (import (level-1)
   expose (telos0
           stream0)
   syntax (boot0))

;;;-----------------------------------------------------------------------------
;;; Let/cc and unwind-protect
;;;-----------------------------------------------------------------------------
(defsyntax let/cc (k . body)
  ;; call/ep must not occur in tail position
  (let ((res (gensym)))
    `(let ((,res (call/ep (named-lambda call/ep-lambda (,k) ,@body))))
       ,res)))

(defsyntax unwind-protect (form . clean-up-forms)
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
(defsyntax block forms
  (cons 'let/cc forms))

(defsyntax return-from (name . forms)
  (list name (cons 'progn forms)))

;;;-----------------------------------------------------------------------------
;;; Catch and throw
;;;-----------------------------------------------------------------------------
(defsyntax catch (tag . body)
  (let ((k (gensym)))
    `(let/cc ,k
       (dynamic-let ((,tag ,k)) ,@body))))

(defsyntax throw (tag . forms)
  `((dynamic ,tag) (progn ,@forms)))

;;;-----------------------------------------------------------------------------
;;; While loop
;;;-----------------------------------------------------------------------------
(defsyntax while (condition . body)
  (let ((loop (gensym)))
    `(let/cc break
       (labels
        ((,loop ()
                (when ,condition
                      ,@body
                      (,loop))))
        (,loop)))))

;;  (defsyntax for (init condition inc . body)
;;    `(progn ,init (while ,condition ,@body ,inc)))

;;;-----------------------------------------------------------------------------
;;; Case
;;;-----------------------------------------------------------------------------
(defsyntax case (keyform . clauses)
  (let ((key (gensym))
        (last-clause (list-ref clauses (- (list-size clauses) 1))))
    (if (null? (eq (car last-clause) 'else))
        ;(ct-warning () "missing else branch in (case ... ~a)" last-clause)
        (format "*** WARNING: missing else branch in (case ... ~a)" last-clause)
      ())
    `(let ((,key ,keyform))
       (cond
         ,@(map
            (lambda (clause)
              (let ((keylist (car clause))
                    (forms (cdr clause)))
                (cond
                  ((and (eq clause last-clause) (eq keylist 'else))
                   `(else ,@forms))
                  ((not (list? keylist))
                   `((eql ,key ',keylist) ,@forms))
                  ((null? keylist)
                   `((null? ,key) ,@forms))
                  ((cdr keylist)
                   `((member ,key ',keylist eql) ,@forms))
                  (else
                   `((eql ,key ',(car keylist)) ,@forms)))))
            clauses)))))

;;;-----------------------------------------------------------------------------
;;; defcondition
;;;-----------------------------------------------------------------------------
(defsyntax defcondition (name super slots . keywords)
  `(defclass ,name ,(or super '<condition>) ,slots ,@keywords))

;;;-----------------------------------------------------------------------------
;;; Dynamic variables
;;    defglobal is top-level form only; must not be used within dynamic-let
;;;-----------------------------------------------------------------------------
(defsyntax defglobal (name val) `(push-dynamic-variable ',name ,val))

(defsyntax dynamic (name) `(dynamic-variable-ref ',name))

(defsyntax dynamic-setq (name val)
  `((setter dynamic-variable-ref) ',name ,val))

(defsyntax dynamic-let (name-vals . body)
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
(defsyntax with-handler (fun . body)
  (let ((res (gensym)))
    `(progn
       (push-error-handler ,fun)
       (let ((,res (progn ,@body)))
         (pop-error-handlers 1)
         ,res))))

;;;-----------------------------------------------------------------------------
;;; Last (will become a generic function)
;;;-----------------------------------------------------------------------------
(defsyntax last (l . number)
  (if (null? number)
      `(list-drop ,l (- (list-size ,l) 1))
    `(list-drop ,l (- (list-size ,l) ,@number))))

;;;-----------------------------------------------------------------------------
;;; Auxiliary functions (not EuLisp)
;;;-----------------------------------------------------------------------------
(defsyntax not (x) `(null? ,x))

(defsyntax butlast (list . number)
  (if (null? number)
      `(reverse-list (list-drop (reverse-list ,list) 1))
    `(reverse-list (list-drop (reverse-list ,list) ,@number))))

(defsyntax time-execution (expr stream)
  (let ((x (gensym "time"))
        (res (gensym "time")))
    `(let* ((,x (cpu-time))
            (,res ,expr))
       (setq ,x (map (lambda (x y)
                       (/ (binary- x y) (convert ticks-per-second <double-float>)))
                     (cpu-time) ,x))
       (sprint ,stream
               "real: "     (vector-ref ,x 0)
               "\nuser: "   (vector-ref ,x 1)
               "\nsystem: " (vector-ref ,x 2)
               nl)
       ,res)))

;;;-----------------------------------------------------------------------------
)  ;; End of module syntax-1
;;;-----------------------------------------------------------------------------
