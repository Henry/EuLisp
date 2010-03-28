;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;; -----------------------------------------------------------------------
;;;                     EuLisp System 'youtoo'
;;; -----------------------------------------------------------------------
;;;  Library: level1 (EuLisp Language Level1 Implementation)
;;;  Authors: Andreas Kind, Julian Padget
;;;  Description: let/cc based on downward call/cc
;;; -----------------------------------------------------------------------
(defmodule let-cc
  (syntax (_macros)
   import (telos thread dynamic)
   export (call/ep))
;;; --------------------------------------------------------------------
;;; Downward call/cc (let/cc defined in _macros)
;;; --------------------------------------------------------------------
  (defun call/ep (fun)
    ;; Vm state not copied; continuations must not escape
    (let ((st (make <state>))
          (prev-no-clean-ups (list-size (dynamic *clean-ups*)))
          (prev-no-error-handlers *current-no-error-handlers*)
          (prev-no-dynamic-variables *current-no-dynamic-variables*))
      (fill-simple-state st)
      ;; fun must not be in tail position, so don't touch this let
      (let ((res (fun (named-lambda k (value)
                        ;; 1. call clean-up funs of unwind-protect
                        (labels
                         ((loop (i ll)
                                (if (int-binary< prev-no-clean-ups i)
                                    (progn
                                      ((car ll))
                                      (loop (int-binary- i 1) (cdr ll)))
                                  ())))
                         (let ((l (dynamic *clean-ups*)))
                           (loop (list-size l) l)))
                        ;; 2. reset error-handlers
                        (pop-error-handlers
                         (int-binary- *current-no-error-handlers*
                                      prev-no-error-handlers))
                        ;; 3. reset dynamic variables
                        (pop-dynamic-variables
                         (int-binary- *current-no-dynamic-variables*
                                      prev-no-dynamic-variables))
                        ;; 4. restore vm state
                        (restore-simple-state st value)))))
        (if res res ()))))
)  ; end of module
