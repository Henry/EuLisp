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
;;; Title: boot syntax
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule _boot0
  (import (level-1))

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

(defmacro when (pred . body) `(if ,pred (progn ,@body) ()))

(defmacro unless (pred . body) `(if ,pred () (progn ,@body)))

;;;-----------------------------------------------------------------------------
;;; Global register access
;;;-----------------------------------------------------------------------------
(defmacro set-global-register (name value)
  `((opencoded-lambda (x)
                      (set-register-ref ,name)
                      (register-ref ,name)) ,value))

(defmacro get-global-register (name)
  `((opencoded-lambda ()
                      (register-ref ,name))))

;;;-----------------------------------------------------------------------------
)  ;; End of module _boot0
;;;-----------------------------------------------------------------------------
