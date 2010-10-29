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
;;; Title: dynamic (i.e. fluid) variables
;;;  Library: level-1
;;;  Authors: Andreas Kind, Julian Padget
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule dynamic
  (syntax (_telos0)
   import (telos
           thread)
   export (push-dynamic-variable
           pop-dynamic-variables
           dynamic-variable-ref
           push-error-handler
           pop-error-handlers
           *current-no-dynamic-variables*
           *current-no-error-handlers*))

;;;-----------------------------------------------------------------------------
;;; Dynamic variable access
;;;-----------------------------------------------------------------------------
(deflocal *current-no-dynamic-variables* 0)

(defun dynamic-variable-ref (name)
  (labels
   ((loop (ll)
          (if (null? ll)
              (error () "dynamic variable ~a not available" name)
            (if (eq name (car ll))
                (car (cdr ll))
              (loop (cdr (cdr ll)))))))
   (loop (thread-dynamic-variables (current-thread)))))

(defun (setter dynamic-variable-ref) (name val)
  (labels
   ((loop (ll)
          (if (null? ll)
              (error () "dynamic variable ~a not available" name)
            (if (eq name (car ll))
                ((setter car) (cdr ll) val)
              (loop (cdr (cdr ll)))))))
   (loop (thread-dynamic-variables (current-thread)))))

(defun push-dynamic-variable (name val)
  (let ((thrd (current-thread)))
    (setq *current-no-dynamic-variables*
          (fpi-binary+ *current-no-dynamic-variables* 1))
    ((setter thread-dynamic-variables) thrd
     (cons name (cons val (thread-dynamic-variables thrd))))))

(defun pop-dynamic-variables (n)
  (labels
   ((loop (nn ll)
          (if (fpi-binary= nn 0) ll
            (loop (fpi-binary- nn 1) (cdr (cdr ll))))))
   (let ((thrd (current-thread)))
     (setq *current-no-dynamic-variables*
           (fpi-binary- *current-no-dynamic-variables* n))
     ((setter thread-dynamic-variables) thrd
      (loop n (thread-dynamic-variables thrd))))))

;;;-----------------------------------------------------------------------------
;;; Error handlers
;;;-----------------------------------------------------------------------------
(deflocal *current-no-error-handlers* 0)

(defun push-error-handler (fun)
  (let ((thrd (current-thread)))
    (setq *current-no-error-handlers*
          (fpi-binary+ *current-no-error-handlers* 1))
    ((setter thread-error-handlers)
     thrd (cons fun (thread-error-handlers thrd)))))

(defun pop-error-handlers (n)
  (labels
   ((loop (nn ll)
          (if (fpi-binary= nn 0) ll
            (loop (fpi-binary- nn 1) (cdr ll)))))
   (let ((thrd (current-thread)))
     (setq *current-no-error-handlers*
           (fpi-binary- *current-no-error-handlers* n))
     ((setter thread-error-handlers) thrd
      (loop n (thread-error-handlers thrd))))))

;;;-----------------------------------------------------------------------------
)  ;; End of module dynamic
;;;-----------------------------------------------------------------------------
