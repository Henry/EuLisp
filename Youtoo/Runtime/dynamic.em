;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: dynamic (i.e. fluid) variables
;;;-----------------------------------------------------------------------------
(defmodule dynamic
  (syntax (_telos0)
   import (telos thread)
   export (push-dynamic-variable pop-dynamic-variables dynamic-variable-ref
                                 push-error-handler pop-error-handlers
                                 *current-no-dynamic-variables* *current-no-error-handlers*))

;;;-----------------------------------------------------------------------------
;;; Dynamic variable access
;;;-----------------------------------------------------------------------------
(deflocal *current-no-dynamic-variables* 0)

(defun dynamic-variable-ref (name)
  (labels
   ((loop (ll)
          (if (null? ll)
              (error "dynamic variable ~a not available" name)
            (if (eq name (car ll))
                (car (cdr ll))
              (loop (cdr (cdr ll)))))))
   (loop (thread-dynamic-variables (current-thread)))))

(defun (setter dynamic-variable-ref) (name val)
  (labels
   ((loop (ll)
          (if (null? ll)
              (error "dynamic variable ~a not available" name)
            (if (eq name (car ll))
                ((setter car) (cdr ll) val)
              (loop (cdr (cdr ll)))))))
   (loop (thread-dynamic-variables (current-thread)))))

(defun push-dynamic-variable (name val)
  (let ((thrd (current-thread)))
    (setq *current-no-dynamic-variables*
          (int-binary+ *current-no-dynamic-variables* 1))
    ((setter thread-dynamic-variables) thrd
     (cons name (cons val (thread-dynamic-variables thrd))))))

(defun pop-dynamic-variables (n)
  (labels
   ((loop (nn ll)
          (if (int-binary= nn 0) ll
            (loop (int-binary- nn 1) (cdr (cdr ll))))))
   (let ((thrd (current-thread)))
     (setq *current-no-dynamic-variables*
           (int-binary- *current-no-dynamic-variables* n))
     ((setter thread-dynamic-variables) thrd
      (loop n (thread-dynamic-variables thrd))))))

;;;-----------------------------------------------------------------------------
;;; Error handlers
;;;-----------------------------------------------------------------------------
(deflocal *current-no-error-handlers* 0)

(defun push-error-handler (fun)
  (let ((thrd (current-thread)))
    (setq *current-no-error-handlers*
          (int-binary+ *current-no-error-handlers* 1))
    ((setter thread-error-handlers)
     thrd (cons fun (thread-error-handlers thrd)))))

(defun pop-error-handlers (n)
  (labels
   ((loop (nn ll)
          (if (int-binary= nn 0) ll
            (loop (int-binary- nn 1) (cdr ll)))))
   (let ((thrd (current-thread)))
     (setq *current-no-error-handlers*
           (int-binary- *current-no-error-handlers* n))
     ((setter thread-error-handlers) thrd
      (loop n (thread-error-handlers thrd))))))

;;;-----------------------------------------------------------------------------
)  ;; end of module
;;;-----------------------------------------------------------------------------
