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
;;; Title: Scheme synatx (IEEE Std 1178-1990) in EuLisp
;;;  Library: Scheme
;;;  Authors: Andreas Kind, Pete Broadbery, Luc Moreau
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule scmtoo0
  (import (level1
           eval))

;;;-----------------------------------------------------------------------------
;;; Trace
;;; Actions are pre/post thunks with the traced function+parameters as
;;; arguments.
;;;-----------------------------------------------------------------------------
(defmacro trace (function-name . actions)
  (let* ((tmp-name (concatenate '| | function-name))
         (pre-action (if actions (car actions) ()))
         (post-action (if (if pre-action
                              (cdr actions)
                            ())
                          (cadr actions)
                        ())))
    `(progn
       (deflocal ,tmp-name ())
       (setq *redefine-imported-bindings*
             (list *redefine-imported-bindings*))
       (setq ,tmp-name ,function-name)
       (setq ,function-name
             (named-lambda
              ,function-name args
              ,(if pre-action
                   `(apply ,pre-action ,function-name args)
                 `(progn
                    (display ">>> ")
                    (display (dynamic-variable-ref '*trace-indent*))
                    (display "TRACE [")
                    (display ',function-name)
                    (display "]: ")
                    (display args)
                    (newline)))
              (let ((res (begin
                          (push-dynamic-variable
                           '*trace-indent*
                           (string-append (dynamic-variable-ref '*trace-indent*)
                                          " "))
                          (apply ,tmp-name args))))
                (pop-dynamic-variables 1)
                ,(if post-action
                     `(apply ,post-action ,function-name args)
                   `(progn
                      (display "<<< ")
                      (display (dynamic-variable-ref '*trace-indent*))
                      (display "TRACE [")
                      (display ',function-name)
                      (display "]: ")
                      (display args)
                      (display " => ")
                      (display res)
                      (newline)))
                res)))
       ;; retrieve previous value
       (setq *redefine-imported-bindings*
             (car *redefine-imported-bindings*))
       ,function-name)))

(defmacro untrace (function-name)
  (let ((tmp-name (concatenate '| | function-name)))
    `(progn
       (setq *redefine-imported-bindings*
             (list *redefine-imported-bindings*))
       (setq ,function-name ,tmp-name)
       ;; retrieve previous value
       (setq *redefine-imported-bindings*
             (car *redefine-imported-bindings*)))))

;;;-----------------------------------------------------------------------------
)  ;; End of module scmtoo0
;;;-----------------------------------------------------------------------------
