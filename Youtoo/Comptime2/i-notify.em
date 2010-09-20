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
;;;  Library: comp (EuLisp to Bytecode Compiler -- EuLysses))
;;;  Authors: Andreas Kind, Keith Playford
;;; Description: compile-time notifications and warnings
;;;-----------------------------------------------------------------------------
(defmodule i-notify
  (syntax (_macros)
   import (i-level1 i-param)
   export (notify0 notify ct-warning ct-serious-warning))

;;;-----------------------------------------------------------------------------
;;;  Notification ...
;;;-----------------------------------------------------------------------------
(defun basic-notify (s var str . args)
  (if (null? var) ()
    (progn
      (apply sformat s str args)
      (snewline s)
      (sflush s))))

(defun notify0 (str . args)
  (apply basic-notify stderr *verbose* str args))

(defun notify (str . args)
  (apply basic-notify stderr (null? *silent*)
         (string-append (dynamic *indent*) str) args))

(defun basic-warning (var prompt str . args)
  (dynamic-let ((*pprint* 1))
               (let* ((m (dynamic *actual-module*))
                      (pre-str (if m
                                   (fmt "[~a]" (primitive-ref m 0))
                                 ""))
                      (fun (get-named-encl-lambda (dynamic *encl-lambda*))))
                 (if (null? fun) ()
                   (setq pre-str (fmt "~a[~a]" pre-str fun)))
                 (apply basic-notify stderr var
                        (fmt "*** ~a ~a: ~a" prompt pre-str str) args))))

;;;-----------------------------------------------------------------------------
;;;  Warning (compilation can be continued)
;;;-----------------------------------------------------------------------------
(defun ct-warning (value str . args)
  (setq *number-of-warnings* (+ *number-of-warnings* 1))
  (apply basic-warning *warnings* "WARNING" str args)
  value)

;;;-----------------------------------------------------------------------------
;;;  Serious warning; compilation can only be continued up to next
;;  check-stop; see ct-error in i-errror for immediate compilation exit
;;;-----------------------------------------------------------------------------
(defun ct-serious-warning (value str . args)
  (setq *number-of-errors* (+ *number-of-errors* 1))
  (stop-after-pass)
  (if (null? *interpreter*)
      (progn
        (apply basic-warning *errors* "ERROR" str args)
        value)
    ;; Force error to be catched by interpreter loop
    (error (get-ct-error-condition-class ())
            (apply fmt str args)
            ct-error-value: (dynamic *actual-module*))))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
