 ;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: Scheme
;;;  Authors: Andreas Kind, Pete Broadbery, Luc Moreau
;;; Description: Scheme synatx (IEEE Std 1178-1990) in EuLisp
;;;-----------------------------------------------------------------------------
(defmodule scmtoo0
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
)  ;; End of module
;;;-----------------------------------------------------------------------------
