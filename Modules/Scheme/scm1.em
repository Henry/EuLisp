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

(defmodule scm0
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
                 `(sformat stderr
                           ,(fmt ">>> ~~aTRACE [~a]: ~~a\n" function-name)
                           (dynamic *trace-indent*) args))
              (let ((res (dynamic-let ((*trace-indent*
                                        (concatenate (dynamic *trace-indent*)
                                                     " ")))
                                      (apply ,tmp-name args))))
                ,(if post-action
                     `(apply ,post-action ,function-name args)
                   `(sformat stderr
                             ,(fmt "<<< ~~aTRACE [~a]: ~~a => ~~a\n"
                                   function-name)
                             (dynamic *trace-indent*) args res))
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
)  ;; End of module scm1
;;;-----------------------------------------------------------------------------
