;;; Copyright (c) 1997 by A Kind & University of Bath. All rights reserved.
;;;-----------------------------------------------------------------------------
;;; ---                         EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;; Description: keywords
;;;-----------------------------------------------------------------------------
(defmodule mop-key
  (syntax (_boot0)
   import (boot)
   export (find-key filter-keywords))

;;;-----------------------------------------------------------------------------
;;; Find a key in a {key val}* keyword list
;;; Syntax: (find-key key keywords default), where default is a value to
;;; return if the key is absent.  If default is required:, an
;;; warning is signalled when the key is absent (see also _mop-gf0)

;;;-----------------------------------------------------------------------------
(defun find-key (key keywords default)
  (let ((val (init-list-ref keywords key *absent*)))
    (if (eq val *absent*)
        (if (eq default required:)
            (progn
              (warning "missing required keyword ~a" key)
              val)
          default)
      val)))

(defun filter-keywords (keywords ignore)
  (labels
   ((loop (keys res)
          (if (null? keys)
              (reverse-list res)
            (let* ((key (car keys))
                   (tmp (cdr keys))
                   (val (car tmp))
                   (rest (cdr tmp)))
              (if (member1-list key ignore)
                  (loop rest res)
                (loop rest (cons val (cons key res))))))))
   (loop keywords ())))

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
