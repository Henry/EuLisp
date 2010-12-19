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
;;; Title: let/cc based on downward call/cc
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule let-cc
  (syntax (_syntax-1)
   import (telos
           thread
           dynamic)
   export (call/ep))

;;;-----------------------------------------------------------------------------
;;; Downward call/cc (let/cc defined in syntax-1)
;;;-----------------------------------------------------------------------------
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
                                          (if (fpi-binary< prev-no-clean-ups i)
                                              (progn
                                                ((car ll))
                                                (loop (fpi-binary- i 1) (cdr ll)))
                                            ())))
                                   (let ((l (dynamic *clean-ups*)))
                                     (loop (list-size l) l)))
                                  ;; 2. reset error-handlers
                                  (pop-error-handlers
                                   (fpi-binary- *current-no-error-handlers*
                                                prev-no-error-handlers))
                                  ;; 3. reset dynamic variables
                                  (pop-dynamic-variables
                                   (fpi-binary- *current-no-dynamic-variables*
                                                prev-no-dynamic-variables))
                                  ;; 4. restore vm state
                                  (restore-simple-state st value)))))
      (if res res ()))))

;;;-----------------------------------------------------------------------------
)  ;; End of module let-cc
;;;-----------------------------------------------------------------------------
