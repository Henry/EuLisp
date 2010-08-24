;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: Scheme
;;;  Authors: Andreas Kind, Pete Broadbery, Luc Moreau
;;; Description: Scheme synatx (IEEE Std 1178-1990) in EuLisp
;;;-----------------------------------------------------------------------------
(defmodule scm0
  (import (level1 eval))

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
       (setq ,function-name (named-lambda ,function-name args
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
)  ;; end of module
;;;-----------------------------------------------------------------------------
