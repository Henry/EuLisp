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
;;; Title: condition
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule condition
  (syntax (_syntax-1)
   import (telos
           thread
           dynamic
           let-cc)
   export (<condition>
           condition?
           condition-message
           <general-condition>
           signal
           error
           *default-error-handler*
           output-condition-contents
           push-error-handler
           pop-error-handlers))

;;;-----------------------------------------------------------------------------
;;; Class <condition> and <general-condition>
;;;-----------------------------------------------------------------------------
(defclass <condition> <object>
  ((message keyword: message:
            accessor: condition-message))
  predicate: condition?)

(defcondition <general-condition> ()
  ((arguments keyword: arguments:)))

;;;-----------------------------------------------------------------------------
;;; Errors and signals
;;;-----------------------------------------------------------------------------
(defun signal (cndt k . thrds)
  (let* ((thrd (or (and thrds (car thrds)) (current-thread)))
         (hdls (thread-error-handlers thrd)))
    (labels
     ((loop (ll)
            (if (null? ll)
                (*default-error-handler* cndt k)
              (progn
                (let/cc handler-error
                  ;; Beware of errors occuring in an error handler
                  (with-handler (lambda (c kk)
                                  (setq cndt c)
                                  (setq k kk)
                                  (handler-error ()))
                                ((car ll) cndt k)))
                ;; Pass signal to next handler if previous returns
                (loop (cdr ll))))))
     (loop hdls))))

;;;-----------------------------------------------------------------------------
;;; Default error handling
;;;-----------------------------------------------------------------------------
(deflocal *default-error-handler*
  (named-lambda default-error-handler (cndt k)
                (output-condition-contents cndt)
                (if (null? k) ()
                  (progn
                    (primitive-format 2 "***    Do you want to continue? (y/n) ")
                    (if (eq (getchar) #\y) (k cndt) ())))
                (primitive-format 2 "***    See Backtrace? (y/n) ")
                (if (eq (getchar) #\y) (backtrace) ())
                (exit -1)))

(defun output-condition-contents (cndt)
  (let* ((cl (class-of cndt))
         (name (class-name cl))
         (str (condition-message cndt)))
    (labels
     ((loop (sds)
            (and sds
                 (let* ((sd (car sds))
                        (name (slot-name sd))
                        (value (slot-value-using-slot sd cndt)))
                   (primitive-format 2 "    ~a: ~a\n" name value)
                   (loop (cdr sds))))))
     (primitive-format 2 "\n*** ERROR [~a]: ~a\n" name str)
     (loop (cdr (class-slots cl))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module condition
;;;-----------------------------------------------------------------------------
