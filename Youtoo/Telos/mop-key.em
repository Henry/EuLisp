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
;;; Title: keywords
;;;  Library: telos (The EuLisp Object System -- TELOS)
;;;  Authors: Russel Bradford, Andreas Kind
;;;  Maintainer: Henry G. Weller
;;;-----------------------------------------------------------------------------

(defmodule mop-key
  (syntax (_boot0)
   import (boot)
   export (find-key
           filter-keywords))

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
)  ;; End of module mop-key
;;;-----------------------------------------------------------------------------
