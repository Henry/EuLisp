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
;;;  Library: level1
;;;  Authors: Andreas Kind, Julian Padget
;;; Description: locks
;;;-----------------------------------------------------------------------------
(defmodule lock
  (syntax (_telos0)
   import (telos thread)
   export (<semaphore> semaphore-counter semaphore?
                       <lock> lock? lock unlock))

;;;-----------------------------------------------------------------------------
;;; Class <lock>
;;;-----------------------------------------------------------------------------
(defclass <semaphore> ()
  ((counter default: 0 accessor: semaphore-counter keyword: counter:))
  predicate: semaphore?
  predicate: lock?)

(deflocal <lock> <semaphore>)

;;;-----------------------------------------------------------------------------
;;; Lock
;;;-----------------------------------------------------------------------------
(defgeneric lock (lk))

(defmethod lock ((lk <lock>))
  (labels
   ((loop ()
          (or (test-and-set-lock lk)
              (progn
                (thread-reschedule)
                (loop)))))
   (loop)))

(defopencoded test-and-set-lock (lk) (test-and-set-lock))

;;;-----------------------------------------------------------------------------
;;; Unlock
;;;-----------------------------------------------------------------------------
(defgeneric unlock (lk))

(defmethod unlock ((lk <lock>))
  ((setter semaphore-counter) lk 0)
  lk)

;;;-----------------------------------------------------------------------------
)  ;; End of module
;;;-----------------------------------------------------------------------------
